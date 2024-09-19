module Lib
  ( entry,
  )
where

import System.Environment (getArgs, withArgs)
import Task.Hant (sequentialExperiment2, parallelExperiment1, parallelExperiment2, coverageExperiment1, coverageExperiment2, nopruningExperiment2)

entry :: IO ()
entry = do
  args <- getArgs
  if length args >= 2
    then case head args of
      "parallel" -> parallelExperiment (drop 1 args)
      "sequential" -> sequentialExperiment (drop 1 args)
      "nopruning" -> nopruningExperiment (drop 1 args)
      "coverage" -> coverageExperiment (drop 1 args)
      _ -> error "invalid arguments"
    else
      error "invalid arguments"

coverageExperiment :: [String] -> IO ()
coverageExperiment args = do
  case head args of
    "experiment1" -> withArgs (drop 1 args) coverageExperiment1
    "experiment2" -> withArgs (drop 1 args) coverageExperiment2
    _ -> error "invalid arguments"

sequentialExperiment :: [String] -> IO ()
sequentialExperiment args = do
  case head args of
    "experiment1" -> error "invalid arguments"
    "experiment2" -> withArgs (drop 1 args) sequentialExperiment2
    _ -> error "invalid arguments"

nopruningExperiment :: [String] -> IO ()
nopruningExperiment args = do
  case head args of
    "experiment1" -> error "invalid arguments"
    "experiment2" -> withArgs (drop 1 args) nopruningExperiment2
    _ -> error "invalid arguments"

parallelExperiment :: [String] -> IO ()
parallelExperiment args = do
  case head args of
    "experiment1" -> withArgs (drop 1 args) parallelExperiment1
    "experiment2" -> withArgs (drop 1 args) parallelExperiment2
    _ -> error "invalid arguments"