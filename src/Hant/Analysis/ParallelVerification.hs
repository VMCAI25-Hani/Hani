module Hant.Analysis.ParallelVerification
  ( Mode(..),
    analyze,
    parallelAnalyzeLiteratureCase,
    analyzeSynthesizedCase,
  )
where

import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.Async (async, Async, cancel, wait)
import Control.Concurrent.STM (TQueue, atomically, newTQueueIO, readTQueue, writeTQueue)
import Control.Monad (forever, replicateM)
import Data.Either (partitionEithers)
import Data.List (isInfixOf)
import Hant.Analysis.Guided (analyzeHanGuidedByTrace)
import Hant.Analysis.Pretty (printCaseName, printIsdStatistics)
import Hant.Analysis.Trace (Trace, traces)
import Hant.Analysis.UnsatCore (unsatCoreToFragment)
import Hant.Analysis.Validation (validateDiagrams)
import Hant.Ast.Diagram (Automaton, Bound, Diagrams, Message)
import Hant.Parser (parseShan)
import Hant.Pretty (blank)
import Hant.Synthesis.Synthesizer (SynthesizedCase (caseId, diagrams))
import Hant.Synthesis.Synthesizer qualified as Synth
import Hant.Util (LiteratureCase (bound, name, path))
import Text.Printf (printf)
import Data.Time (getCurrentTime, diffUTCTime)
import Control.Exception (try, SomeException)

data Mode = Sequential | Parallel | NoPruning
  deriving (Eq, Show)

parallelAnalyzeLiteratureCase :: LiteratureCase -> IO ()
parallelAnalyzeLiteratureCase c = do
  startTime <- getCurrentTime
  printCaseName (name c)
  sdOrAutomaton <- parseShan (path c)
  let diags = partitionEithers sdOrAutomaton
  analyze (bound c) diags Parallel
  endTime <- getCurrentTime
  let diff = diffUTCTime endTime startTime
  putStrLn $ "Time consumption: " ++ show diff

analyzeSynthesizedCase :: SynthesizedCase -> Mode -> IO ()
analyzeSynthesizedCase c m = do
  startTime <- getCurrentTime
  printCaseName (caseId c)
  let diags = diagrams c
  let b = Synth.bound c
  analyze b diags m
  endTime <- getCurrentTime
  let diff = diffUTCTime endTime startTime
  putStrLn $ "Time consumption: " ++ show diff

analyze :: Bound -> Diagrams -> Mode -> IO ()
analyze b (sds, han) m = do
  let validationRes = validateDiagrams (sds, han)
  case validationRes of
    [] ->
      let ts = concatMap traces sds
       in do
            printIsdStatistics b sds ts han
            case m of
              Sequential -> sequentialAnalyzeHanGuidedByTraces b han ts
              Parallel -> parallelAnalyzeHanGuidedByTraces b han ts
              NoPruning -> noPruningAnalyzeHanGuidedByTraces b han ts
    errorMsg -> print errorMsg

sequentialAnalyzeHanGuidedByTraces ::
  Bound ->
  [Automaton] ->
  [Trace] ->
  IO ()
sequentialAnalyzeHanGuidedByTraces b han ts = do
  taskQueue <- newTQueueIO
  checkResultQueue <- newTQueueIO
  workers <- replicateM 1 $ async $ worker b han taskQueue checkResultQueue
  sequentialInitialize ts taskQueue checkResultQueue workers

parallelAnalyzeHanGuidedByTraces ::
  Bound ->
  [Automaton] ->
  [Trace] ->
  IO ()
parallelAnalyzeHanGuidedByTraces b han ts = do
  taskQueue <- newTQueueIO
  checkResultQueue <- newTQueueIO
  capabilityCount <- getNumCapabilities
  let num = if length ts < 10
              then capabilityCount
              else 1
  workers <- replicateM num $ async $ worker b han taskQueue checkResultQueue
  initializePruner ts taskQueue checkResultQueue workers

noPruningAnalyzeHanGuidedByTraces ::
  Bound ->
  [Automaton] ->
  [Trace] ->
  IO ()
noPruningAnalyzeHanGuidedByTraces b han ts = do
  taskQueue <- newTQueueIO
  checkResultQueue <- newTQueueIO
  capabilityCount <- getNumCapabilities
  mapM_ (atomically . writeTQueue taskQueue) ts
  workers <- replicateM capabilityCount $ async $ worker b han taskQueue checkResultQueue
  checker (length ts) 0 checkResultQueue workers

checker ::
  Int ->
  Int ->
  TQueue (Either [Message] String) ->
  [Async ()] ->
  IO ()
checker total done resultQueue workers =
  if total == done
    then do
      putStrLn "verified"
      blank
    else do
      checkResult <- atomically $ readTQueue resultQueue
      case checkResult of
        Left _ -> checker total (done + 1) resultQueue workers
        Right counterExample -> do
          mapM_ cancel workers
          mapM_ ((try :: IO a -> IO (Either SomeException a)) . wait) workers
          putStrLn "Counter Example: "
          putStrLn counterExample
          blank

pruner ::
  Int ->
  [Trace] ->
  TQueue Trace ->
  TQueue (Either [Message] String) ->
  [Async ()] ->
  IO ()
pruner processingTasks tasks taskQueue checkResultQueue workers = do
  if processingTasks == 0
    then do
      putStrLn "verified"
      blank
    else do
      checkResult <- atomically $ readTQueue checkResultQueue
      case checkResult of
        Left fragment -> do
          let filtered = filter (not . isInfixOf fragment) tasks
          let pruned = length tasks - length filtered
          let prompt = if pruned == 0
                        then "complete 1 task"
                        else printf "complete 1 task, prune %d tasks" pruned
          putStrLn prompt
          let remaining =
                if null filtered
                  then processingTasks - 1
                  else processingTasks
          mapM_ (atomically . writeTQueue taskQueue) (take 1 filtered)
          pruner remaining (drop 1 filtered) taskQueue checkResultQueue workers
        Right counterExample -> do
          mapM_ cancel workers
          mapM_ ((try :: IO a -> IO (Either SomeException a)) . wait) workers
          putStrLn "Counter Example: "
          putStrLn counterExample
          blank

initializePruner ::
  [Trace] ->
  TQueue Trace ->
  TQueue (Either [Message] String) ->
  [Async ()] ->
  IO ()
initializePruner tasks taskQueue checkResultQueue workers = do
  let initialTasks = take (length workers) tasks
  let initialTaskCount = length initialTasks
  mapM_ (atomically . writeTQueue taskQueue) initialTasks
  pruner initialTaskCount (drop initialTaskCount tasks) taskQueue checkResultQueue workers

sequentialInitialize ::
  [Trace] ->
  TQueue Trace ->
  TQueue (Either [Message] String) ->
  [Async ()] ->
  IO ()
sequentialInitialize tasks taskQueue checkResultQueue workers = do
  let initialTasks = take 1 tasks
  let initialTaskCount = length initialTasks
  mapM_ (atomically . writeTQueue taskQueue) initialTasks
  pruner initialTaskCount (drop initialTaskCount tasks) taskQueue checkResultQueue workers

worker ::
  Bound ->
  [Automaton] ->
  TQueue Trace ->
  TQueue (Either [Message] String) ->
  IO ()
worker b han taskQueue checkResultQueue = forever $ do
  trace <- atomically $ readTQueue taskQueue
  res <- analyzeHanGuidedByTrace b han trace
  case res of
    Left unsatCore -> do
      let fragment = unsatCoreToFragment trace unsatCore
      atomically $ writeTQueue checkResultQueue (Left fragment)
    Right counterExample -> do
      atomically $ writeTQueue checkResultQueue (Right counterExample)