module Hant.Analysis.Coverage
  (
    coverageLiteratureCase,
    coverageSynthesizedCase
  )
where

import Data.Set (Set)
import Data.Set qualified as S
import Hant.Util (LiteratureCase (name, path, bound))
import Hant.Synthesis.Synthesizer (SynthesizedCase (caseId, diagrams))
import Hant.Synthesis.Synthesizer qualified as Synth
import Hant.Analysis.Pretty (printCaseName, countHanModesAndTransitions)
import Hant.Parser (parseShan)
import Data.Either (partitionEithers)
import Hant.Analysis.Trace (traces, Trace)
import Hant.Ast.Diagram (Bound, Automaton, Name, edgeMap, namedHan)
import Data.SBV (runSMT)
import Hant.Analysis.Memo (emptyMemo)
import Hant.Analysis.Guided (encodeAutomataGuidedByTrace)
import Control.Monad.State (evalStateT)
import Data.SBV.Control (query, getModel, checkSat, CheckSatResult (Sat))
import Hant.Analysis.Decoder (decodeModel, Transitions, Transition (Transition), TransitionState (currentLocation))
import Data.Text (Text, pack, append)
import Data.Maybe (mapMaybe)
import Data.Map qualified as M

coverageLiteratureCase :: LiteratureCase -> IO ()
coverageLiteratureCase c = do
  printCaseName (name c)
  sdOrAutomaton <- parseShan (path c)
  let (sds, automata) = partitionEithers sdOrAutomaton
  let b = bound c
  let (totalModes, totalTransitions) = countHanModesAndTransitions automata
  let ts = take 1 (concatMap traces sds)
  mapped <- mapM (coverageOfHanPerTrace b automata) ts
  let (modes, transitions) = foldl (\(m, t) (m', t') -> (S.union m m', S.union t t')) (S.empty, S.empty) mapped
  putStrLn $ "First trace length: " ++ show (length . head $ ts) 
  putStrLn $ "Total modes: " ++ show totalModes
  putStrLn $ "Total transitions: " ++ show totalTransitions
  putStrLn $ "Covered modes: " ++ show (S.size modes)
  putStrLn $ "Covered transitions: " ++ show (S.size transitions)
  putStrLn $ "Mode Coverage: " ++ show (fromIntegral (S.size modes) / fromIntegral totalModes :: Double)
  putStrLn $ "Transition Coverage: " ++ show (fromIntegral (S.size transitions) / fromIntegral totalTransitions :: Double)

coverageSynthesizedCase :: SynthesizedCase -> IO ()
coverageSynthesizedCase c = do
  printCaseName (caseId c)
  let (sds, automata) = diagrams c
  let b = Synth.bound c
  let (totalModes, totalTransitions) = countHanModesAndTransitions automata
  let ts = take 10 (concatMap traces sds)
  mapped <- mapM (coverageOfHanPerTrace b automata) ts
  let (modes, transitions) = foldl (\(m, t) (m', t') -> (S.union m m', S.union t t')) (S.empty, S.empty) mapped
  putStrLn $ "Total modes: " ++ show totalModes
  putStrLn $ "Total transitions: " ++ show totalTransitions
  putStrLn $ "Covered modes: " ++ show (S.size modes)
  putStrLn $ "Covered transitions: " ++ show (S.size transitions)
  putStrLn $ "Mode Coverage: " ++ show (fromIntegral (S.size modes) / fromIntegral totalModes :: Double)
  putStrLn $ "Transition Coverage: " ++ show (fromIntegral (S.size transitions) / fromIntegral totalTransitions :: Double)

coverageOfHanPerTrace :: Bound -> [Automaton] -> Trace -> IO (Set Text, Set Text)
coverageOfHanPerTrace b han t = do
  runSMT $ do
    evalStateT (encodeAutomataGuidedByTrace b han t) (emptyMemo han)
    query $ do
      satRes <- checkSat
      case satRes of
        Sat -> do
          model <- getModel
          let namedTrans = decodeModel han model
          return (countModes namedTrans, countTransitions namedTrans han)
        _ -> do
          return (S.empty, S.empty)

countTransitions :: [(Name, Transitions)] -> [Automaton] -> Set Text
countTransitions namedTrans han =
  let hanMap = namedHan han
      count (n, ts) = case M.lookup n hanMap of
        Just a -> countTransitionPerAutomaton (n, ts) a
        Nothing -> []
   in S.fromList $ concatMap count namedTrans

countTransitionPerAutomaton :: (Name, Transitions) -> Automaton -> [Text]
countTransitionPerAutomaton (n, ts) a =
  let transMap = edgeMap a
      transPair = pairs $ transitionMode <$> ts
   in append n <$> mapMaybe (`M.lookup` transMap) transPair

countModes :: [(Name, Transitions)] -> Set Text
countModes namedTrans = S.fromList $ concatMap countModesPerAutomaton namedTrans

countModesPerAutomaton :: (Name, Transitions) -> [Text]
countModesPerAutomaton (n, ts) =
  [append n (transitionMode t) | t <- ts]

transitionMode :: Transition -> Text
transitionMode (Transition _ _ s) = pack (currentLocation s)

pairs :: [a] -> [(a, a)]
pairs [] = []
pairs xs = zip xs (tail xs)