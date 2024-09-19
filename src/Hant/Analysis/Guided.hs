{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Redundant bracket" #-}

module Hant.Analysis.Guided
  ( analyzeLiteratureCase,
    analyzeSynthesizedCase,
    analyzeHanGuidedByTrace,
    encodeAutomataGuidedByTrace,
    tickLiteratureCase
  )
where

import Control.Monad.State (MonadState (get, put), MonadTrans (lift), evalStateT)
import Data.Either (partitionEithers)
import Data.SBV (OrdSymbolic, SBool, SReal, SWord8, SymVal (literal), namedConstraint, runSMT, sAnd, sNot, sOr, sReal, sTrue, sWord8, setOption, (.&&), (./=), (.<), (.<=), (.==), (.>), (.>=), (.||), (.=>))
import Data.SBV.Control (CheckSatResult (..), SMTOption (..), checkSat, getUnknownReason, getUnsatCore, query, getModel)
import Data.Set (Set, (\\))
import Data.Set qualified as S
import Hant.Analysis.LocMap (LocMap, llookup)
import Hant.Analysis.Memo (Memo (locLiteralMap), SymMemo, emptyMemo, insertDuration, insertLocation, insertSyncTime, insertSyncValue, insertVariable, lookupDuration, lookupLocation, lookupSyncTime, lookupSyncValue, lookupVariable)
import Hant.Analysis.Pretty (printCaseName, printIsdStatistics)
import Hant.Analysis.Trace (Direction (..), Index, LMessage, LTrace, Trace, projection, selectEvent, showTrace, traces)
import Hant.Analysis.UnsatCore (initialName, propertiesName, pruneTracesViaUnsatCore, segmentName)
import Hant.Analysis.Validation (validateDiagrams)
import Hant.Ast.Diagram (Assignment (..), Automaton (Automaton), Bound, Dexpr (..), Diagrams, Differential (..), Edge (..), Event (Event), Expr (..), JudgeOp (..), Judgement (..), Message (Message), Name, Node (Node), Property (Property), Reachability (..), Variable, aname, automatonInitialEdges, automatonVars, nname, nonInitialEdges, selectEdgeByName)
import Hant.Parser (parseShan)
import Hant.Pretty (separationLine)
import Hant.Synthesis.Synthesizer (SynthesizedCase (caseId, diagrams))
import Hant.Synthesis.Synthesizer qualified as Synth
import Hant.Util (LiteratureCase (..))
import Text.Printf (printf)
import Hant.Analysis.Offset (segmentStartIndex, segmentEndpoints, locationIndices, indexTrace, oneToEndIdx, initialLocationIndex, initialSegmentIndex)

import Hant.Analysis.Encoder (durationVarName, nodeVarName, variableVarName, syncTimeVarName, syncValueVarName)
import Hant.Analysis.Decoder (explain)
import Data.Time (getCurrentTime, UTCTime, diffUTCTime)

tickLiteratureCase :: LiteratureCase -> IO ()
tickLiteratureCase c = do
  printCaseName (name c)
  sdOrAutomaton <- parseShan (path c)
  let (sds, han) = partitionEithers sdOrAutomaton
  let ts = take 100 (concatMap traces sds)
  let b = bound c
  startTime <- getCurrentTime
  mapM_ (\t -> oneTick b han t startTime) ts

oneTick :: Bound -> [Automaton] -> Trace -> UTCTime -> IO ()
oneTick b han t startTime = do 
  runSMT verificationQuery
  endTime <- getCurrentTime
  let diff = diffUTCTime endTime startTime
  putStrLn $ "Time consumption: " ++ show diff
  where
    verificationQuery = do
      evalStateT (encodeAutomataWithProperties b han t) (emptyMemo han)
      query $ do
        satRes <- checkSat
        case satRes of
          Unsat -> return ()
          Sat -> error "sat"
          DSat Nothing -> error "delta satisfiable"
          DSat (Just s) -> error $ "delta satisfiable: " ++ show s
          Unk -> do
            reason <- getUnknownReason
            error $ "unknown: " ++ show reason

analyzeLiteratureCase :: LiteratureCase -> IO ()
analyzeLiteratureCase c = do
  startTime <- getCurrentTime
  printCaseName (name c)
  sdOrAutomaton <- parseShan (path c)
  let diags = partitionEithers sdOrAutomaton
  validateThenAnalyze (bound c) diags
  endTime <- getCurrentTime
  let diff = diffUTCTime endTime startTime
  putStrLn $ "Time consumption: " ++ show diff

analyzeSynthesizedCase :: SynthesizedCase -> IO ()
analyzeSynthesizedCase c = do
  startTime <- getCurrentTime
  printCaseName (caseId c)
  let diags = diagrams c
  let b = Synth.bound c
  validateThenAnalyze b diags
  endTime <- getCurrentTime
  let diff = diffUTCTime endTime startTime
  putStrLn $ "Time consumption: " ++ show diff

validateThenAnalyze :: Bound -> Diagrams -> IO ()
validateThenAnalyze b (sds, han) = do
  let validationRes = validateDiagrams (sds, han)
  case validationRes of
    [] -> let ts = concatMap traces sds
           in do
                printIsdStatistics b sds ts han
                analyzeHanGuidedByTraces b han ts
    errs -> print errs
      
analyzeHanGuidedByTraces :: Bound -> [Automaton] -> [Trace] -> IO ()
analyzeHanGuidedByTraces _ _ [] = putStrLn "verified"
analyzeHanGuidedByTraces b ms (t : ts) = do
  putStrLn $ showTrace t
  res <- analyzeHanGuidedByTrace b ms t
  case res of
    Left unsatCore -> do
      putStrLn $ "Unsat Core: " ++ show unsatCore
      let pruned = pruneTracesViaUnsatCore ts t unsatCore
      putStrLn $ printf "prune %d paths" (length ts - length pruned)
      separationLine
      analyzeHanGuidedByTraces b ms pruned
    Right counterExample -> putStrLn counterExample

analyzeHanGuidedByTrace :: Bound -> [Automaton] -> Trace -> IO (Either [String] String)
analyzeHanGuidedByTrace b han t = do
  runSMT querySmtVerificationResult
  where
    querySmtVerificationResult = do
      evalStateT (encodeAutomataWithProperties b han t) (emptyMemo han)
      setSolvingOption
      query $ do
        satRes <- checkSat
        case satRes of
          Unsat -> Left <$> getUnsatCore
          Sat -> do
            model <- getModel
            return (Right (showTrace t ++ "\n" ++ explain b han model))
          DSat Nothing -> error "delta satisfiable"
          DSat (Just s) -> error $ "delta satisfiable: " ++ show s
          Unk -> do
            reason <- getUnknownReason
            error $ "unknown: " ++ show reason
    setSolvingOption = do
      setOption $ ProduceUnsatCores True
      setOption $ OptionKeyword ":smt.core.minimize" ["true"]

encodeAutomataWithProperties :: Bound -> [Automaton] -> Trace -> SymMemo ()
encodeAutomataWithProperties b ms t = do
  encodeAutomataGuidedByTrace b ms t
  propertiesInNegation <- encodePropertiesInNegation b ms t
  lift $ namedConstraint propertiesName propertiesInNegation

encodePropertiesInNegation :: Bound -> [Automaton] -> Trace -> SymMemo SBool
encodePropertiesInNegation b han t =
  sNot . sAnd <$> traverse automatonProperties han
  where
    automatonProperties (Automaton machineName _ _ _ ps) =
      sAnd <$> traverse singleProperty ps
      where
        locations = locationIndices b (length t)
        singleProperty (Property node Reachable) = sOr <$> traverse (localize' machineName node) locations
        singleProperty (Property node Unreachable) = sAnd <$> traverse (unlocalize' machineName node) locations

encodeAutomataGuidedByTrace :: Bound -> [Automaton] -> Trace -> SymMemo ()
encodeAutomataGuidedByTrace b han t = mapM_ (\m -> encodeAutomatonGuidedByTrace b m t) han

encodeAutomatonGuidedByTrace :: Bound -> Automaton -> Trace -> SymMemo ()
encodeAutomatonGuidedByTrace b m t = encodeAutomataGuidedByLTrace b m (projection t m)

encodeAutomataGuidedByLTrace :: Bound -> Automaton -> LTrace -> SymMemo ()
encodeAutomataGuidedByLTrace b m ltrace = do
  initializeSegment b m
  encodeSegments b m ltrace

encodeSegments :: Bound -> Automaton -> LTrace -> SymMemo ()
encodeSegments b m ltrace = do 
  let indexedLTrace = indexTrace ltrace
  mapM_ (encodeSegment b m) indexedLTrace

encodeSegment :: Bound -> Automaton -> (Maybe LMessage, Index) -> SymMemo ()
encodeSegment b m (mlm, i) = do
  let n = aname m
  let startIndex = segmentStartIndex b i
  startOfSegmentIsSynchronousJump <- synchronousJump m startIndex (mlm, i)
  timeIsSynchronized <- synchronizeTime n startIndex (mlm, i)
  afterSynchronousJumpIsLocalTransitions <- localTransitions b i m
  lift $
    namedConstraint
      (segmentName n i)
      ( startOfSegmentIsSynchronousJump
          .&& timeIsSynchronized
          .&& afterSynchronousJumpIsLocalTransitions
      )

localTransitions :: Bound -> Index -> Automaton -> SymMemo SBool
localTransitions b indexOfSyncEvent m = do
  let start = segmentStartIndex b indexOfSyncEvent
  let (l, r) = segmentEndpoints start b
  sAnd <$> traverse (transition m) [l .. r]

synchronizeTime :: Name -> Index -> (Maybe LMessage, Index) -> SymMemo SBool
synchronizeTime n endIdx (mlm, i) = case mlm of
    Nothing -> return sTrue
    Just (Message mn _ _ _, _) -> do 
      synchronousMessage <- synchronousTimeVar mn i
      untilNow <- sumOfCostTime
      return (synchronousMessage .== untilNow)
  where
    sumOfCostTime = sum <$> traverse (duration n) (oneToEndIdx endIdx)

transition :: Automaton -> Index -> SymMemo SBool
transition m i = do
  jumpToSomeNode <- jump m i
  stayInCurrentNode <- timed m i
  return (jumpToSomeNode .|| stayInCurrentNode)

initializeSegment :: Bound -> Automaton -> SymMemo ()
initializeSegment b m = do
  automatonIsSetToInitialStates <- selectInitialSegment b m
  lift $ namedConstraint (initialName m) automatonIsSetToInitialStates

selectInitialSegment :: Bound -> Automaton -> SymMemo SBool
selectInitialSegment b m =
  sOr <$> traverse initialEdge (automatonInitialEdges m)
  where
    n = aname m
    initialEdge (Edge _ _ t _ as) = do
      memo <- get
      let lmap = locLiteralMap memo
      l <- location n initialLocationIndex
      let locationIsSetViaInitialEdge = l .== literalLocation lmap n (nname t)
      variablesAreAssigned <- assignments n as initialLocationIndex
      afterInitialIsLocalTransitions <- localTransitions b initialSegmentIndex m
      timeCoseIsZero <- noTimeCost n initialLocationIndex
      return (locationIsSetViaInitialEdge 
              .&& timeCoseIsZero
              .&& variablesAreAssigned
              .&& afterInitialIsLocalTransitions)

-- all the variables in the automaton remain unchanged, i.e. v_{i-1} == v_i
unchanged :: Name -> Set Variable -> Index -> SymMemo SBool
unchanged n vs i =
  sAnd <$> clauses
  where
    clauses = traverse unchanged' (S.toList vs)
    unchanged' :: Variable -> SymMemo SBool
    unchanged' v = do
      pre <- indexedVar n v (i - 1)
      suc <- indexedVar n v i
      return (pre .== suc)

-- update the variables in the automaton according to the assignments
assignments :: Name -> [Assignment] -> Index -> SymMemo SBool
assignments n as i =
  sAnd <$> traverse assignment as
  where
    assignment (Assignment v e) = do
      pre <- encodeExpr n e (max (i - 1) 0)
      suc <- indexedVar n v i
      return (suc .== pre)

judgement :: Name -> Index -> Judgement -> SymMemo SBool
judgement n i = judgement'
  where
    judgement' (SimpleJ le op re) = do
      left <- encodeExpr n le i
      right <- encodeExpr n re i
      return (judge op left right)
    judgement' (AndJ j1 j2) = do
      j1' <- judgement' j1
      j2' <- judgement' j2
      return (j1' .&& j2')
    judgement' (OrJ j1 j2) = do
      j1' <- judgement' j1
      j2' <- judgement' j2
      return (j1' .|| j2')

-- the invariants in the timed transition
-- TODO: ensure the invariant in the whole time duration
invariants :: Name -> [Judgement] -> Index -> SymMemo SBool
invariants n js i = sAnd <$> traverse (judgement n i) js

-- update the variables in the automaton according to the flow conditions
flow :: Name -> [Differential] -> Index -> SymMemo SBool
flow n ds i =
  sAnd <$> clauses
  where
    clauses = traverse flow' ds
    flow' (Differential v op de) = do
      left <- delta v
      right <- encodeDexpr de
      return (judge op left right)
    delta v = do
      vi <- indexedVar n v i
      vi' <- indexedVar n v (i - 1)
      return (vi - vi')
    encodeDexpr :: Dexpr -> SymMemo SReal
    encodeDexpr de =
      case de of
        Dnumber d -> do 
          d' <- duration n i
          let realVal = literal . fromRational $ toRational d
          return (d' * realVal)
        Nvar v -> indexedVar n v i
        Dvar v -> delta v
        Dnegation de' -> negate <$> encodeDexpr de'
        Dadd de1 de2 -> (+) <$> encodeDexpr de1 <*> encodeDexpr de2
        Dsub de1 de2 -> (-) <$> encodeDexpr de1 <*> encodeDexpr de2
        Dmul de1 de2 -> (*) <$> encodeDexpr de1 <*> encodeDexpr de2
        Ddiv de1 de2 -> (/) <$> encodeDexpr de1 <*> encodeDexpr de2

guard :: Name -> Maybe Judgement -> Index -> SymMemo SBool
guard _ Nothing _ = return sTrue
guard n (Just j) i = judgement n i j

-- encoding a jumping transition in an automaton
jump :: Automaton -> Index -> SymMemo SBool
jump m@(Automaton n _ _ es _) i = do
  timeCostIsZero <- noTimeCost n i
  allTargetArePossible <- sOr <$> traverse (encodeEdge m i) (nonInitialEdges es)
  return (timeCostIsZero .&& allTargetArePossible)

-- encoding a stutter transition in an automaton
stutter :: Automaton -> Index -> SymMemo SBool
stutter m@(Automaton n _ _ _ _) i = do
  timeCostIsZero <- noTimeCost n i
  locationRemainsUnchanged <- noLocationTransition n i
  allVariablesWillNotChange <- unchanged n (automatonVars m) i
  return (timeCostIsZero 
          .&& locationRemainsUnchanged
          .&& allVariablesWillNotChange)

encodeEdge :: Automaton -> Index -> Edge -> SymMemo SBool
encodeEdge m@(Automaton n _ _ _ _) i (Edge _ s t g as) = do
  previousLocIsSource <- localize n (i - 1) s
  nextLocIsTarget <- localize n i t
  guardIsTrue <- guard n g (i - 1)
  variablesAreAssigned <- assignments n as i
  restVariablesRemainUnchanged <- unchanged n (automatonVars m \\ updatedVars as) i
  return
    ( previousLocIsSource
        .&& nextLocIsTarget
        .&& guardIsTrue
        .&& variablesAreAssigned
        .&& restVariablesRemainUnchanged
    )

synchronousJump :: Automaton -> Index -> (Maybe LMessage, Index) -> SymMemo SBool
synchronousJump m i (Nothing, _) = stutter m i
synchronousJump m i (Just lm, mi) = do
  let (Event en _) = selectEvent lm
  let edge = selectEdgeByName en m
  synchronousJump' m i edge (lm, mi)

synchronousJump' :: Automaton -> Index -> Edge -> (LMessage, Index) -> SymMemo SBool
synchronousJump' m@(Automaton n _ _ _ _) i (Edge _ s t g as) ((Message mn _ _ sync, d), mi) = do
  previousLocIsSource <- localize n (i - 1) s
  nextLocIsTarget <- localize n i t
  guardIsTrue <- guard n g (i - 1)
  variablesAreAssigned <- case d of
    Sending -> assignments n as i
    Receiving -> assignments n assignmentsNotShadowedBySync i
  variablesAreSynchronized <- sAnd <$> traverse (synchronize d) (zip sync [0 ..])
  restVariablesRemainUnchanged <- case d of
    Sending -> unchanged n (automatonVars m \\ updatedVars as) i
    Receiving -> unchanged n (automatonVars m \\ updatedVars as \\ syncVars) i
  timeCostIsZero <- noTimeCost n i
  return
    ( previousLocIsSource
        .&& nextLocIsTarget
        .&& guardIsTrue
        .&& variablesAreAssigned
        .&& variablesAreSynchronized
        .&& restVariablesRemainUnchanged
        .&& timeCostIsZero
    )
  where
    syncVars = updatedVars sync
    assignmentsNotShadowedBySync = filter (\(Assignment an _) -> an `notElem` syncVars) as
    synchronize Sending (Assignment _ expr, ai) = do
      mv <- synchronousValueVar mn mi ai
      ev <- encodeExpr n expr i
      return (mv .== ev)
    synchronize Receiving (Assignment v _, ai) = do
      assigned <- indexedVar n v i
      mv <- synchronousValueVar mn mi ai
      return (assigned .== mv)

localize :: Name -> Index -> Node -> SymMemo SBool
localize n i node = do
  memo <- get
  let lmap = locLiteralMap memo
  l <- location n i
  return (l .== literalLocation lmap n (nname node))

localize' :: Name -> Name -> Index -> SymMemo SBool
localize' machineName nodeName i = do
  memo <- get
  let lmap = locLiteralMap memo
  l <- location machineName i
  return (l .== literalLocation lmap machineName nodeName)

unlocalize' :: Name -> Name -> Index -> SymMemo SBool
unlocalize' n nodeName i = do
  memo <- get
  let lmap = locLiteralMap memo
  l <- location n i
  return (l ./= literalLocation lmap n nodeName)

-- encode a timed transition in an automaton
timed :: Automaton -> Index -> SymMemo SBool
timed m@(Automaton n _ ns _ _) i = do
  timeCostIsGreaterThanZero <- hasTimeCost n i
  locationRemainsUnchanged <- noLocationTransition n i
  allNodesEvolveAccordingToFlow <- sAnd <$> traverse ifSelectThisNode ns
  return
    ( timeCostIsGreaterThanZero
        .&& locationRemainsUnchanged
        .&& allNodesEvolveAccordingToFlow
    )
  where
    selectNode nodeName = do
      memo <- get
      let lmap = locLiteralMap memo
      loc <- location n i
      return (loc .== literalLocation lmap n nodeName)
    ifSelectThisNode (Node _ nodeName _ diffs invars) = do
      nodeIsSelected <- selectNode nodeName
      variablesAreUpdatedAccordingToFlow <- flow n diffs i
      restVariablesRemainUnchanged <- unchanged n (automatonVars m \\ evovledVars diffs) i
      invariantsHold <- invariants n invars i
      let evolveAccordingToThisNode = variablesAreUpdatedAccordingToFlow
                                      .&& restVariablesRemainUnchanged
                                      .&& invariantsHold
      return (nodeIsSelected .=> evolveAccordingToThisNode)

noTimeCost :: Name -> Index -> SymMemo SBool
noTimeCost n i = do
  d <- duration n i
  return (d .== 0)

hasTimeCost :: Name -> Index -> SymMemo SBool
hasTimeCost n i = do
  d <- duration n i
  return (d .> 0)

-- declare a fresh symbolic variable
-- to represent the time costed by the i-th transition
duration :: Name -> Index -> SymMemo SReal
duration n i = do
  memo <- get
  case lookupDuration memo n i of
    Just d -> return d
    Nothing -> do
      d <- lift $ sReal (durationVarName n i)
      let memo' = insertDuration memo n i d
      put memo'
      return d

noLocationTransition :: Name -> Index -> SymMemo SBool
noLocationTransition n i = do
  preL <- location n (i - 1)
  succL <- location n i
  return (preL .== succL)

-- declare a fresh symbolic variable
-- to represent the node state after the i-th transition
location :: Name -> Index -> SymMemo SWord8
location n i = do
  memo <- get
  case lookupLocation memo n i of
    Just l -> return l
    Nothing -> do
      l <- lift $ sWord8 (nodeVarName n i)
      let memo' = insertLocation memo n i l
      put memo'
      return l

indexedVar :: Name -> Variable -> Index -> SymMemo SReal
indexedVar n v i = do
  memo <- get
  case lookupVariable memo n v i of
    Just d -> return d
    Nothing -> do
      d <- lift $ sReal (variableVarName n v i)
      let memo' = insertVariable memo n v i d
      put memo'
      return d

-- declare a fresh symbolic variable
-- to represent the time of the i-th synchronous event
synchronousTimeVar :: Name -> Index -> SymMemo SReal
synchronousTimeVar n i = do
  memo <- get
  case lookupSyncTime memo n i of
    Just d -> return d
    Nothing -> do
      d <- lift $ sReal (syncTimeVarName n i)
      let memo' = insertSyncTime memo n i d
      put memo'
      return d

-- declare a fresh symbolic variable
-- to represent the exchanged value of i'-th assignment of the i-th synchronous event
synchronousValueVar :: Name -> Index -> Index -> SymMemo SReal
synchronousValueVar n i i' = do
  memo <- get
  case lookupSyncValue memo n i i' of
    Just d -> return d
    Nothing -> do
      d <- lift $ sReal (syncValueVarName n i i')
      let memo' = insertSyncValue memo n i i' d
      put memo'
      return d

encodeExpr :: Name -> Expr -> Index -> SymMemo SReal
encodeExpr n e i =
  case e of
    Number d -> return (literal . fromRational $ toRational d)
    Var v -> indexedVar n v i
    Negation e' -> negate <$> encodeExpr n e' i
    Add e1 e2 -> (+) <$> encodeExpr n e1 i <*> encodeExpr n e2 i
    Sub e1 e2 -> (-) <$> encodeExpr n e1 i <*> encodeExpr n e2 i
    Mul e1 e2 -> (*) <$> encodeExpr n e1 i <*> encodeExpr n e2 i
    Div e1 e2 -> (/) <$> encodeExpr n e1 i <*> encodeExpr n e2 i

updatedVars :: [Assignment] -> Set Variable
updatedVars = S.fromList . fmap (\(Assignment v _) -> v)

evovledVars :: [Differential] -> Set Variable
evovledVars = S.fromList . fmap (\(Differential v _ _) -> v)

judge :: OrdSymbolic a => JudgeOp -> a -> a -> SBool
judge Ge  = (.>=)
judge Gt  = (.>)
judge Le  = (.<=)
judge Lt  = (.<)
judge Eq  = (.==)
judge Neq = (./=)

literalLocation :: LocMap -> Name -> Name -> SWord8
literalLocation lmap n nn = literal (lmap `llookup` (n, nn))