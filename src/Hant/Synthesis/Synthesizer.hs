{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Avoid lambda using `infix`" #-}

module Hant.Synthesis.Synthesizer
  ( SynthesisConfig (..),
    SynthesizedCase (..),
    genCase,
    synthesizeCases,
  )
where

import Control.Lens (Lens', makeLenses, (%~), (&), (.~), (^.))
import Control.Monad.Reader (ReaderT (runReaderT), ask, replicateM)
import Control.Monad.State (MonadState (get), State, evalState, put)
import Data.List (nub, (\\), delete)
import Data.Set qualified as S
import Data.Map qualified as M
import Data.Text (pack)
import Hant.Ast.Diagram (Assignment (Assignment), Automaton (Automaton), Dexpr (Dnumber), Diagrams, Differential (Differential), Edge (Edge), Event (Event), Expr (Number, Var), Fragment (..), Instance (Instance), IntFragment (..), Item (..), JudgeOp (..), Judgement (SimpleJ), Message (Message), Name, Node (Node), NodeType (Common, Initial), Property (Property), Reachability (Reachable, Unreachable), SequenceDiagram (SequenceDiagram), Variable, aname, edgesToNodes, ename, nonInitEdges)
import System.Random (StdGen, UniformRange, mkStdGen, uniform, uniformR)
import Text.Printf (printf)
import Data.List.NonEmpty (groupBy)
import Data.List.NonEmpty qualified as N

type Seed = StdGen

type Index = Int

data SynthesisConfig = SynthesisConfig
  { _caseNum :: Int,
    _initialSeed :: Int,
    _checkingBound :: Int,
    _componentRange :: (Int, Int),
    _nodeRange :: (Int, Int),
    _edgeRange :: (Int, Int),
    _initialEdgeRange :: (Int, Int),
    _variableCountRange :: (Int, Int),
    _variableCountWithinNodeRange :: (Int, Int),
    _variableCountWithinAssignmentRange :: (Int, Int),
    _propertyCountRange :: (Int, Int),
    _constantRange :: (Double, Double),
    _itemCountRange :: (Int, Int),
    _loopBoundRange :: (Int, Int),
    _intCountRange :: (Int, Int),
    _intBoundRange :: (Int, Int),
    _priorityRange :: (Int, Int),
    _maxLayer :: Int
  }
  deriving (Show, Eq)

data SynthesizedCase = SynthesizedCase
  { caseId :: String,
    diagrams :: Diagrams,
    bound :: Int
  }
  deriving (Show, Eq)

data Context = Context
  { _instances :: [Instance],
    _automata :: [Automaton]
  }
  deriving (Show, Eq)

data SynthesisState = SynthesisState
  { _seed :: Seed,
    _layer :: Int,
    _messageIdx :: Index
  }
  deriving (Show, Eq)

type Synthesis a = ReaderT SynthesisConfig (State SynthesisState) a

makeLenses ''SynthesisConfig
makeLenses ''SynthesisState
makeLenses ''Context

synthesizeCases :: SynthesisConfig -> [SynthesizedCase]
synthesizeCases config =
  let caseCount = config ^. caseNum
   in (\i -> genCase i config) <$> [1 .. caseCount]

genCase :: Index -> SynthesisConfig -> SynthesizedCase
genCase i config =
  let caseSeed = (config ^. initialSeed) + i
      sstate = SynthesisState (mkStdGen caseSeed) 0 0
      diagram = evalState (runReaderT genDiagram config) sstate
      b = config ^. checkingBound
   in SynthesizedCase (printf "gen%d" i) diagram b

genDiagram :: Synthesis Diagrams
genDiagram = do
  han <- genAutomata
  isds <- genSequenceDiagrams han
  return (isds, han)

-- this synthesizer only generates one sequence diagram
genSequenceDiagrams :: [Automaton] -> Synthesis [SequenceDiagram]
genSequenceDiagrams han = do
  isd <- genSequenceDiagram 1 han
  return [isd]

genSequenceDiagram :: Index -> [Automaton] -> Synthesis SequenceDiagram
genSequenceDiagram i han = do
  let sdName = pack $ printf "isd%d" i
  let components = genInstances han
  frag <- genFragment $ Context components han
  return $ SequenceDiagram sdName components frag []

genFragment :: Context -> Synthesis Fragment
genFragment context = do
  currentLayer <- genLayer
  if currentLayer == 0
    then genBlock
    else genControlFrag context
  where
    genBlock = do
      items <- genItems context
      ints <- genIntFragments context
      return $ Block (items ++ (ItemF <$> ints))

genIntFragments :: Context -> Synthesis [Fragment]
genIntFragments context = do
  intCount <- randomIntCount
  replicateM intCount (genIntFragment context)

genIntFragment :: Context -> Synthesis Fragment
genIntFragment context = do
  p <- randomPriority
  (l, u) <- randomIntBounds
  items <- genItems context
  return $ IntF $ IntFragment p l u items

genItems :: Context -> Synthesis [Item]
genItems context = do
  itemCount <- randomItemCount
  replicateM itemCount (genItem context)

genItem :: Context -> Synthesis Item
genItem context = do
  currentLayer <- getLayer
  config <- ask
  let maxL = config ^. maxLayer
  if currentLayer >= maxL
    then genMessageItem
    else randomItemOrFrag
  where
    genMessageItem = ItemM <$> genMessage context
    randomItemOrFrag = do
      b <- randomBool
      if b
        then ItemM <$> genMessage context
        else ItemF <$> genFragment context

genControlFrag :: Context -> Synthesis Fragment
genControlFrag context = do
  b <- randomBool
  if b
    then genAltFragment context
    else genLoopFragment context

genAltFragment :: Context -> Synthesis Fragment
genAltFragment context = do
  ifBranch <- genItems context
  elseBranch <- genItems context
  return $ AltF ifBranch elseBranch

genLoopFragment :: Context -> Synthesis Fragment
genLoopFragment context = do
  (l, u) <- randomLoopBounds
  content <- genItems context
  return $ LoopF l u Nothing Nothing content

genMessage :: Context -> Synthesis Message
genMessage context = do
  msgIdx <- genMessageIdx
  (s, t) <- randomSelectTwoUnique (context ^. instances)
  sevent <- genEvent context s
  tevent <- genEvent context t
  -- pairs <- selectionSyncVariables context s t
  -- let assignments = genSyncAssignments pairs
  return $ Message (pack (printf "m%d" msgIdx)) sevent tevent []

genEvent :: Context -> Instance -> Synthesis Event
genEvent c ins = do
  let names = getEventByInstance c ins
  eventName <- randomSelectOne names
  return $ Event eventName ins

genInstances :: [Automaton] -> [Instance]
genInstances han =
  let instanceCount = length han
   in (\i -> genInstance i (han !! (i - 1))) <$> [1 .. instanceCount]

genInstance :: Int -> Automaton -> Instance
genInstance i automaton =
  Instance automatonName (pack (printf "ins%d" i))
  where
    automatonName = aname automaton

genAutomata :: Synthesis [Automaton]
genAutomata = do
  componentCount <- randomComponentCount
  traverse genAutomaton [1 .. componentCount]

genAutomaton :: Index -> Synthesis Automaton
genAutomaton i = do
  let automatonName = pack $ printf "automaton%d" i
  let initialNode = genInitialNode
  vars <- genVariablesWithinAutomaton
  commonNodes <- genCommonNodes vars
  commonEdges <- genEdges commonNodes vars
  let zeroOuts = zeroOutDegree commonEdges
  outSupplementEdges <- genOutSupplementEdges zeroOuts commonNodes vars
  let zeroIns = zeroInDegree commonEdges
  inSupplementEdges <- genInSupplementEdges zeroIns commonNodes vars
  initialEdges <- genInitialEdges initialNode commonNodes vars
  let es = initialEdges ++ filterReduntantEdges (commonEdges ++ outSupplementEdges ++ inSupplementEdges)
  let ns = filter (/= initialNode) (edgesToNodes es)
  properties <- genProperties ns
  return $ Automaton automatonName initialNode ns es properties

filterReduntantEdges :: [Edge] -> [Edge]
filterReduntantEdges = map N.head . groupBy sameEndpoints
  where
    sameEndpoints (Edge _ s t _ _) (Edge _ s' t' _ _) = s == s' && t == t'

zeroInDegree :: [Edge] -> [Node]
zeroInDegree es =
  let target (Edge _ _ t _ _) = t
      allNodes = nub $ concatMap endpoints es
      targets = nub $ target <$> es
   in allNodes \\ targets

zeroOutDegree :: [Edge] -> [Node]
zeroOutDegree es =
  let source (Edge _ s _ _ _) = s
      allNodes = nub $ concatMap endpoints es
      sources = nub $ source <$> es
   in allNodes \\ sources

endpoints :: Edge -> [Node]
endpoints (Edge _ s t _ _) = [s, t]

genCommonNodes :: [Variable] -> Synthesis [Node]
genCommonNodes vars = do
  nodeCount <- randomNodeCount
  traverse (\i -> genCommonNode i vars) [1 .. nodeCount]

genProperties :: [Node] -> Synthesis [Property]
genProperties nodes = do
  count <- randomPropertyCount
  nodes' <- randomSelect nodes count
  traverse genProperty nodes'

genProperty :: Node -> Synthesis Property
genProperty (Node _ name _ _ _) = do
  reachability <- randomReachability
  return $ Property name reachability

genInitialEdges :: Node -> [Node] -> [Variable] -> Synthesis [Edge]
genInitialEdges initialNode nodes vars = do
  count <- randomInitialEdgeCount
  traverse (\i -> genInitialEdge i initialNode nodes vars) [1 .. count]

genInitialEdge :: Index -> Node -> [Node] -> [Variable] -> Synthesis Edge
genInitialEdge i initialNode nodes vars = do
  let edgeName = pack $ printf "ie%d" i
  target <- randomSelectOne nodes
  assignments <- genAssignments vars target
  return $ Edge edgeName initialNode target Nothing assignments

genEdges :: [Node] -> [Variable] -> Synthesis [Edge]
genEdges nodes vars = do
  edgeCount <- randomEdgeCount
  traverse genIthEdge [1 .. edgeCount]
  where
    genIthEdge i = do
      (source, target) <- randomSelectTwoUnique nodes
      genEdge (pack $ printf "e%d" i) source target vars

genInSupplementEdges :: [Node] -> [Node] -> [Variable] -> Synthesis [Edge]
genInSupplementEdges initialNodes nodes vars = do
  traverse genIthSupplement (zip [1::Integer ..] initialNodes)
  where
    genIthSupplement (i, n) = do
      let rest = delete n nodes
      s <- randomSelectOne rest
      let edgeName = pack $ printf "te%d" i
      genEdge edgeName s n vars

genOutSupplementEdges :: [Node] -> [Node] -> [Variable] -> Synthesis [Edge]
genOutSupplementEdges deadends nodes vars = do
  traverse genIthSupplement (zip [1::Integer ..] deadends)
  where
    genIthSupplement (i, n) = do
      let rest = delete n nodes
      t <- randomSelectOne rest
      let edgeName = pack $ printf "se%d" i
      genEdge edgeName n t vars

genEdge :: Name -> Node -> Node -> [Variable] -> Synthesis Edge
genEdge edgeName source target vars = do
      -- guardVarCount <- randomVarCountWithinGuard
      -- varsInGuard <- randomSelect vars guardVarCount
      -- guard <- genGuard varsInGuard
      assignVarCount <- randomVarCountWithinAssignment
      varsInAssign <- randomSelect vars assignVarCount
      let invars = invariantVars target
      let assignVars = nub (varsInAssign ++ invars)
      assignments <- genAssignments assignVars target
      return $ Edge edgeName source target Nothing assignments

-- In synthesized automaton, it's possible that the previous state breaks the invariant of the target node,
-- so we use assignments at the edge to ensure the invariants of target node are satisfied.
genAssignments :: [Variable] -> Node -> Synthesis [Assignment]
genAssignments vs n = traverse genInitialAssignment vs
  where
    cmap = constraintMap n
    genInitialAssignment v = do
      config <- ask
      let (l, u) = config ^. constantRange
      let val = M.lookup v cmap
      case val of
        Nothing -> do
          constant <- randomDouble (l, u)
          return $ Assignment v (Number constant)
        Just (Left d) -> do
          constant <- randomDouble (l, d)
          return $ Assignment v (Number constant)
        Just (Right d) -> do
          constant <- randomDouble (d, u)
          return $ Assignment v (Number constant)

constraintMap :: Node -> M.Map Variable (Either Double Double)
constraintMap (Node _ _ _ _ js) = M.fromList $ extract <$> js
  where
    extract (SimpleJ (Var v) Ge (Number d)) = (v, Right d)
    extract (SimpleJ (Var v) Gt (Number d)) = (v, Right d)
    extract (SimpleJ (Var v) Le (Number d)) = (v, Left d)
    extract (SimpleJ (Var v) Lt (Number d)) = (v, Left d)
    extract _ = error "impossible"

invariantVars :: Node -> [Variable]
invariantVars (Node _ _ _ _ js) = extract <$> js
  where
    extract (SimpleJ (Var v) _ _) = v
    extract _ = error "impossible"

genInitialNode :: Node
genInitialNode = Node Initial "" S.empty [] []

genCommonNode :: Index -> [Variable] -> Synthesis Node
genCommonNode i vars = do
  let nodeType = Common
  let nodeName = pack $ printf "node%d" i
  selectedVars <- selectVariablesWithinAutomaton vars
  diffs <- genDifferentialEquations selectedVars
  invs <- genInvariants selectedVars
  return $ Node nodeType nodeName (S.fromList selectedVars) diffs invs

genInvariants :: [Variable] -> Synthesis [Judgement]
genInvariants = traverse genInvariant

genInvariant :: Variable -> Synthesis Judgement
genInvariant v = do
  op <- randomJudgeOp
  constant <- randomConstant
  return $ SimpleJ (Var v) op (Number constant)

genDifferentialEquations :: [Variable] -> Synthesis [Differential]
genDifferentialEquations = traverse genDifferentialEquation

genDifferentialEquation :: Variable -> Synthesis Differential
genDifferentialEquation v = do
  constant <- randomConstant
  let simpleExpr = Dnumber constant
  return $ Differential v Eq simpleExpr

selectVariablesWithinAutomaton :: [Variable] -> Synthesis [Variable]
selectVariablesWithinAutomaton vars = do
  varCount <- randomVarCountWithinNode
  let lst = vars
  randomSelect lst varCount

genVariablesWithinAutomaton :: Synthesis [Variable]
genVariablesWithinAutomaton = do
  count <- randomVariableCount
  return $ map genAutomataVariable [1 .. count]

genAutomataVariable :: Int -> Variable
genAutomataVariable = pack . printf "x%d"

randomSelectOne :: [a] -> Synthesis a
randomSelectOne lst = do
  selection <- randomSelect lst 1
  return $ head selection

randomSelect :: [a] -> Int -> Synthesis [a]
randomSelect lst n = do
  let len = length lst
  indices <- randomNumbers n (0, len - 1)
  let indices' = nub indices
  return [lst !! i | i <- indices']

randomSelectTwoUnique :: Eq a => [a] -> Synthesis (a, a)
randomSelectTwoUnique lst = do
  selection <- randomSelect lst 2
  if length selection < 2
    then randomSelectTwoUnique lst
    else do
      let x1 = selection !! 0
      let x2 = selection !! 1
      if x1 == x2
        then randomSelectTwoUnique lst
        else return (x1, x2)

randomConstant :: Synthesis Double
randomConstant = randomByRange constantRange

randomComponentCount :: Synthesis Int
randomComponentCount = randomByRange componentRange

randomNodeCount :: Synthesis Int
randomNodeCount = randomByRange nodeRange

randomEdgeCount :: Synthesis Int
randomEdgeCount = randomByRange edgeRange

randomInitialEdgeCount :: Synthesis Int
randomInitialEdgeCount = randomByRange initialEdgeRange

randomVariableCount :: Synthesis Int
randomVariableCount = randomByRange variableCountRange

randomVarCountWithinAssignment :: Synthesis Int
randomVarCountWithinAssignment = randomByRange variableCountWithinAssignmentRange

randomPropertyCount :: Synthesis Int
randomPropertyCount = randomByRange propertyCountRange

randomItemCount :: Synthesis Int
randomItemCount = randomByRange itemCountRange

randomIntCount :: Synthesis Int
randomIntCount = randomByRange intCountRange

randomPriority :: Synthesis Int
randomPriority = randomByRange priorityRange

randomByRange ::
  UniformRange a =>
  Lens' SynthesisConfig (a, a) ->
  Synthesis a
randomByRange rangeLens = do
  config <- ask
  let range = config ^. rangeLens
  sstate <- get
  let (val, g) = uniformR range (sstate ^. seed)
  put (sstate & seed .~ g)
  return val

randomLoopBounds :: Synthesis (Int, Int)
randomLoopBounds = do
  config <- ask
  let (l, u) = config ^. loopBoundRange
  sstate <- get
  let (val1, g1) = uniformR (l, u) (sstate ^. seed)
  let (val2, g2) = uniformR (l, val1) g1
  put (sstate & seed .~ g2)
  return (val2, val1)

randomIntBounds :: Synthesis (Int, Int)
randomIntBounds = do
  config <- ask
  let (l, u) = config ^. intBoundRange
  sstate <- get
  let (val1, g1) = uniformR (l, u) (sstate ^. seed)
  let (val2, g2) = uniformR (l, val1) g1
  put (sstate & seed .~ g2)
  return (val2, val1)

randomDouble :: (Double, Double) -> Synthesis Double
randomDouble range = do
  sstate <- get
  let (val, g) = uniformR range (sstate ^. seed)
  put (sstate & seed .~ g)
  return val

genLayer :: Synthesis Int
genLayer = do
  sstate <- get
  let l = sstate ^. layer
  put (sstate & layer %~ (+ 1))
  return l

getLayer :: Synthesis Int
getLayer = do
  sstate <- get
  return $ sstate ^. layer

genMessageIdx :: Synthesis Int
genMessageIdx = do
  sstate <- get
  let idx = sstate ^. messageIdx
  put (sstate & messageIdx %~ (+ 1))
  return idx

randomJudgeOp :: Synthesis JudgeOp
randomJudgeOp = do
  sstate <- get
  let (val, g) = uniformR (1 :: Int, 4) (sstate ^. seed)
  put (sstate & seed .~ g)
  return $ intToOp val
  where
    intToOp i = case i of
      1 -> Ge
      2 -> Gt
      3 -> Le
      4 -> Lt
      _ -> error "impossible"

randomReachability :: Synthesis Reachability
randomReachability = do
  sstate <- get
  let (val, g) = uniformR (1 :: Int, 2) (sstate ^. seed)
  put (sstate & seed .~ g)
  return $ intToReachability val
  where
    intToReachability i = case i of
      1 -> Reachable
      2 -> Unreachable
      _ -> error "impossible"

randomVarCountWithinNode :: Synthesis Int
randomVarCountWithinNode = do
  config <- ask
  let range = config ^. variableCountWithinNodeRange
  sstate <- get
  let (val, g) = uniformR range (sstate ^. seed)
  put (sstate & seed .~ g)
  return val

randomNumbers :: Int -> (Int, Int) -> Synthesis [Int]
randomNumbers n range = replicateM n (randomNumber range)

randomNumber :: (Int, Int) -> Synthesis Int
randomNumber range = do
  sstate <- get
  let (val, g) = uniformR range (sstate ^. seed)
  put (sstate & seed .~ g)
  return val

randomBool :: Synthesis Bool
randomBool = do
  sstate <- get
  let (val, g) = uniform (sstate ^. seed)
  put (sstate & seed .~ g)
  return val

getEventByInstance :: Context -> Instance -> [Name]
getEventByInstance c (Instance n _) =
  let automaton = head $ filter (\a -> aname a == n) (c ^. automata)
      events = ename <$> nonInitEdges automaton
   in events