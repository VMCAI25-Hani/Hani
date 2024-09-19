{-# HLINT ignore "Use <$>" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Hant.Analysis.Decoder
  ( explain,
    decodeModel,
    Transition(Transition),
    Transitions,
    TransitionState(currentLocation)
  )
where

import Control.Monad (void)
import Data.List (groupBy, sort, sortOn)
import Data.Map (Map, fromList, toList)
import Data.Maybe (mapMaybe)
import Data.SBV.Internals (AlgReal (..), CV (cvVal), CVal (..), SMTModel (modelAssocs))
import Data.Text (pack, unpack)
import Hant.Analysis.Encoder (durationVarStr, moduleSeparation, nodeVarStr, syncTimeVarStr, syncValueVarStr)
import Hant.Analysis.LocMap (ReverseMap, constructMap, reverseMap, rlookup)
import Hant.Analysis.Pretty (arrowStr, arrowWithInfoStr, emptyBox, padding, seal, (|++|), invisibleBox, syncArrorStr, syncArrowWithInfoStr)
import Hant.Analysis.Trace (Index)
import Hant.Ast.Diagram (Automaton, Name, Variable, Bound)
import Hant.Ast.Diagram.Parser (nameParser, variableParser)
import Hant.Util (Parser, symbolS)
import Text.Megaparsec (MonadParsec (try), choice, errorBundlePretty, manyTill, parse)
import Text.Megaparsec.Char (alphaNumChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Control.Arrow (second)
import Hant.Analysis.Offset (segmentLength)

instance Show CVal where
  show (CAlgReal _) = "CAlgReal"
  show (CInteger _) = "CInteger"
  show (CFloat _) = "CFloat"
  show (CDouble _) = "CDouble"
  show (CFP _) = "CFP"
  show (CRational _) = "CRational"
  show (CChar _) = "CChar"
  show (CString _) = "CString"
  show (CList _) = "CList"
  show (CSet _) = "CSet"
  show (CUserSort _) = "CUserSort"
  show (CTuple _) = "CTuple"
  show (CMaybe _) = "CMaybe"
  show (CEither _) = "CEither"

data TransitionState = TransitionState
  { currentLocation :: String,
    currentValues :: Map Variable Double
  }
  deriving (Eq, Show)

data Arrow
  = Jumping
  | Timing Double
  deriving (Eq, Show)

data Transition
  = Transition Index Arrow TransitionState
  deriving (Eq, Show)

instance Ord Transition where
  compare (Transition i _ _) (Transition i' _ _) = compare i i'

type Transitions = [Transition]

data Interpretation
  = Loc Loc
  | Delta Delta
  | Var Var
  | SyncTime SyncTime
  | SyncValue SyncValue
  deriving (Eq, Show)

type Loc = (Name, Index, String)

type Delta = (Name, Index, Double)

type Var = (Name, Variable, Index, Double)

type SyncTime = (Name, Index, Double)

type SyncValue = (Name, Index, Index, Double)

data InterpretationPrefix
  = LocP Name Index
  | DeltaP Name Index
  | VarP Name Variable Index
  | SyncTimeP Name Index
  | SyncValueP Name Index Index
  deriving (Eq, Show)

toLoc :: Interpretation -> Maybe Loc
toLoc (Loc l) = Just l
toLoc _ = Nothing

toDelta :: Interpretation -> Maybe Delta
toDelta (Delta d) = Just d
toDelta _ = Nothing

toVar :: Interpretation -> Maybe Var
toVar (Var v) = Just v
toVar _ = Nothing

isSync :: Interpretation -> Bool
isSync (SyncTime {}) = True
isSync (SyncValue {}) = True
isSync _ = False

isNotSync :: Interpretation -> Bool
isNotSync = not . isSync

name :: Interpretation -> Name
name (Loc (n, _, _)) = n
name (Delta (n, _, _)) = n
name (Var (n, _, _, _)) = n
name (SyncTime (n, _, _)) = n
name (SyncValue (n, _, _, _)) = n

index :: Interpretation -> Index
index (Loc (_, i, _)) = i
index (Delta (_, i, _)) = i
index (Var (_, _, i, _)) = i
index (SyncTime (_, i, _)) = i
index (SyncValue (_, i, _, _)) = i

inSameModule :: Interpretation -> Interpretation -> Bool
inSameModule i i' = name i == name i'

atSamePosition :: Interpretation -> Interpretation -> Bool
atSamePosition i i' = index i == index i'

explain :: Bound -> [Automaton] -> SMTModel -> String
explain b han model = renderTransitionsOfHan b $ decodeModel han model

renderTransitionsOfHan :: Bound -> [(Name, Transitions)] -> String
renderTransitionsOfHan b =
  foldl concatTransitions ""
  where
    concatTransitions acc (n, ts) = acc ++ "\n" ++ unpack n ++ ":\n" ++ renderTransitions b ts

renderTransitions :: Bound -> Transitions -> String
renderTransitions b ts =
  if null ts
    then ""
    else render "" (segmentLength b) indexedTs
  where
    render acc _ [] = acc
    render acc tryLen remains = 
      case tryChunk (take tryLen remains) of
        Left len -> render acc len remains
        Right r -> render (acc ++ r) (segmentLength b) (drop tryLen remains)
    indexedTs = zip ts [1 ..]
    renderChunk c = 
      let (_, i) = head c
       in if i == 1
            then foldl renderAndConcat emptyBox c
            else foldl renderAndConcat invisibleBox c
    renderAndConcat acc t = acc |++| renderTransition t
    maxWidth = 150
    tryChunk :: [(Transition, Int)] -> Either Int String
    tryChunk c = 
      let r = renderChunk c
          w = length . head . lines $ r
       in if w > maxWidth
            then Left (length c - 1)
            else Right r
    renderTransition (Transition _ arrow state, i) = if (i /= 1) && (i `mod` segmentLength b == 1)
                                                      then renderSyncArrow arrow |++| renderState state
                                                      else renderArrow arrow |++| renderState state
    renderArrow Jumping = arrowStr
    renderArrow (Timing d) = arrowWithInfoStr (show d)
    renderSyncArrow Jumping = syncArrorStr
    renderSyncArrow (Timing d) = syncArrowWithInfoStr (show d)
    renderState (TransitionState loc vals) = seal . padding $ unlines (loc : renderVals vals)
    renderVals vals = map renderEntry (toList vals)
    renderEntry (var, val) = unpack var ++ "=" ++ show val

decodeModel :: [Automaton] -> SMTModel -> [(Name, Transitions)]
decodeModel han model =
  let lmap = constructMap han
      rmap = reverseMap lmap
      interpretations = parseInterpretations rmap (modelAssocs model)
      (modules, _) = partitionByModule interpretations
      namedModules = map (\m -> (name $ head m, m)) modules
      convert = sort . map toTransition . partitionByIndex
      dynamics = second convert <$> namedModules
   in dynamics

toTransition :: [Interpretation] -> Transition
toTransition interpretations =
  let delta = head $ mapMaybe toDelta interpretations
      loc = head $ mapMaybe toLoc interpretations
      vars = mapMaybe toVar interpretations
      i = index $ head interpretations
      arrow = case delta of
        (_, _, d) | d > 0 -> Timing d
        _ -> Jumping
      locVal = third loc
      varMap = fromList $ map (\(_, v, _, val) -> (v, val)) vars
      state = TransitionState locVal varMap
   in Transition i arrow state

partitionByIndex :: [Interpretation] -> [[Interpretation]]
partitionByIndex = groupBy atSamePosition . sortOn index

partitionByModule :: [Interpretation] -> ([[Interpretation]], [Interpretation])
partitionByModule interpretations =
  let syncs = filter isSync interpretations
      nonSyncs = filter isNotSync interpretations
      modules = groupBy inSameModule $ sortOn name nonSyncs
   in (modules, syncs)

parseInterpretations :: ReverseMap -> [(String, CV)] -> [Interpretation]
parseInterpretations rmap = map (parseInterpretation rmap)

parseInterpretation :: ReverseMap -> (String, CV) -> Interpretation
parseInterpretation rmap (s, cv) = case parse interpretationPrefixParser "" (pack s) of
  Left e -> error $ "parse interpretation prefix failed\n" ++ errorBundlePretty e
  Right ip -> prefixToInterpretation rmap ip (cvVal cv)

prefixToInterpretation ::
  ReverseMap ->
  InterpretationPrefix ->
  CVal ->
  Interpretation
prefixToInterpretation rmap (LocP n i) (CInteger v) =
  Loc (n, i, unpack (rmap `rlookup` (n, fromInteger v)))
prefixToInterpretation _ (DeltaP n i) (CAlgReal v) = Delta (n, i, algRealToDouble v)
prefixToInterpretation _ (VarP n vn i) (CAlgReal v) = Var (n, vn, i, algRealToDouble v)
prefixToInterpretation _ (SyncTimeP n i) (CAlgReal v) = SyncTime (n, i, algRealToDouble v)
prefixToInterpretation _ (SyncValueP n i i') (CAlgReal v) = SyncValue (n, i, i', algRealToDouble v)
prefixToInterpretation _ b c = error $ "impossible\n" ++ "prefix: " ++ show b ++ "\n" ++ "value: " ++ show c

algRealToDouble :: AlgReal -> Double
algRealToDouble (AlgRational _ r) = fromRational r
algRealToDouble (AlgPolyRoot (i, _) _) = fromInteger i
algRealToDouble (AlgInterval _ _) = error "AlgInterval"

interpretationPrefixParser :: Parser InterpretationPrefix
interpretationPrefixParser = do
  choice
    [ try locParser,
      try deltaParser,
      try syncTimeParser,
      try syncValueParser,
      varParser
    ]
  where
    locParser = do
      void (symbolS $ pack nodeVarStr)
      void (symbolS $ pack moduleSeparation)
      n <- automatonNameParser
      i <- indexParser
      return $ LocP n i
    deltaParser = do
      void (symbolS $ pack durationVarStr)
      void (symbolS $ pack moduleSeparation)
      n <- automatonNameParser
      i <- indexParser
      return $ DeltaP n i
    varParser = do
      n <- automatonNameParser
      var <- variableParser
      void (symbolS $ pack moduleSeparation)
      i <- indexParser
      return $ VarP n var i
    syncTimeParser = do
      void (symbolS $ pack syncTimeVarStr)
      void (symbolS $ pack moduleSeparation)
      msgName <- nameParser
      void (symbolS $ pack moduleSeparation)
      i <- indexParser
      return $ SyncTimeP msgName i
    syncValueParser = do
      void (symbolS $ pack syncValueVarStr)
      void (symbolS $ pack moduleSeparation)
      msgName <- nameParser
      void (symbolS $ pack moduleSeparation)
      i <- indexParser
      void (symbolS $ pack moduleSeparation)
      i' <- indexParser
      return $ SyncValueP msgName i i'

automatonNameParser :: Parser Name
automatonNameParser = do
  n <- manyTill alphaNumChar (symbolS $ pack moduleSeparation)
  return $ pack n

indexParser :: Parser Index
indexParser = decimal

third :: (a, b, c) -> c
third (_, _, c) = c