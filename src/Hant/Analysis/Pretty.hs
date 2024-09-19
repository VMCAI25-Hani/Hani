module Hant.Analysis.Pretty
  ( modelValues,
    printCaseName,
    printIsdStatistics,
    emptyBox,
    invisibleBox,
    arrowStr,
    syncArrorStr,
    arrowWithInfoStr,
    syncArrowWithInfoStr,
    seal,
    padding,
    (|++|),
    countHanModesAndTransitions
  )
where

import Data.SBV.Internals (SMTModel (modelAssocs))
import Hant.Analysis.Trace (Trace)
import Hant.Ast.Diagram (SequenceDiagram, sdname, splitSequenceDiagram, Automaton (Automaton), nodeCount, edgeCount, Bound, variables)
import Text.Printf (printf)

modelValues :: SMTModel -> String
modelValues m =
  unlines (showAssoc <$> assocs)
  where
    assocs = modelAssocs m
    showAssoc (s, v) = printf "%s = %s" (show s) (show v)

printCaseName :: String -> IO ()
printCaseName n = do
  let nameLen = length n
  putStrLn $ replicate (nameLen + 2) '-'
  putStrLn ("|" ++ n ++ "|")
  putStrLn $ replicate (nameLen + 2) '-'

printIsdStatistics :: Bound -> [SequenceDiagram] -> [Trace] -> [Automaton] -> IO ()
printIsdStatistics b sds ts han = do
  printBound b
  printComponentCount han
  printNumOfProperties han
  printHanNodesAndEdges han
  printHanVariables han
  printIntCount sds
  printTraceCount (length ts)

countHanModesAndTransitions :: [Automaton] -> (Int, Int)
countHanModesAndTransitions han = (sum (nodeCount <$> han), sum (edgeCount <$> han))

printBound :: Bound -> IO ()
printBound b = do
  putStrLn $ "bound: " ++ show b

printComponentCount :: [Automaton] -> IO ()
printComponentCount han = do
  putStrLn $ "component count: " ++ show (length han)

printNumOfProperties :: [Automaton] -> IO ()
printNumOfProperties han = do
  putStrLn $ "number of properties: " ++ show (sum (numOfSingle <$> han))
  where
    numOfSingle (Automaton _ _ _ _ ps) = length ps

printHanNodesAndEdges :: [Automaton] -> IO ()
printHanNodesAndEdges han = do
  let nodes = sum (nodeCount <$> han)
  let edges = sum (edgeCount <$> han)
  putStrLn $ "han node count: " ++ show nodes
  putStrLn $ "han edge count: " ++ show edges

printHanVariables :: [Automaton] -> IO ()
printHanVariables han = do
  let vars = concatMap variables han
  putStrLn $ "han variable count: " ++ show (length vars)

printIntCount :: [SequenceDiagram] -> IO ()
printIntCount sds = do
  mapM_ printIntCount' sds
  where
    printIntCount' :: SequenceDiagram -> IO ()
    printIntCount' sd = do
      let (_, ints) = splitSequenceDiagram sd
      putStrLn $ printf "interrupt count of %s: %d" (sdname sd) (length ints)

printTraceCount :: Int -> IO ()
printTraceCount n = do
  putStrLn $ "test cases: " ++ show n

emptyBox :: String
emptyBox = "┌───┐\n│   │\n└───┘"

invisibleBox :: String
invisibleBox = "     \n     \n     "

arrowStr :: String
arrowStr = "--->"

syncArrorStr :: String
syncArrorStr = "===>"

arrowWithInfoStr :: String -> String
arrowWithInfoStr info =
  let len = length info
      fstLine = info ++ " \n"
      sndLine = replicate len '-' ++ ">"
   in fstLine ++ sndLine

syncArrowWithInfoStr :: String -> String
syncArrowWithInfoStr info =
  let len = length info
      fstLine = info ++ " \n"
      sndLine = replicate len '=' ++ ">"
   in fstLine ++ sndLine

seal :: String -> String
seal info =
  let len = maximum $ map length (lines info)
      fstLine = "┌" ++ replicate len '─' ++ "┐\n"
      midLines = unlines $ map (\l -> "│" ++ l ++ "│") (lines info)
      lstLine = "└" ++ replicate len '─' ++ "┘"
   in fstLine ++ midLines ++ lstLine

padding :: String -> String
padding str =
  let len = maximum $ map length (lines str)
      lines' = map (\l -> l ++ replicate (len - length l) ' ') (lines str)
   in unlines lines'

(|++|) :: String -> String -> String
left |++| right =
  let padLines sideLines sideLen targetHeight =
        let paddingLine = replicate sideLen ' '
            sideHeight = length sideLines
            topPaddingNum = (targetHeight - sideHeight) `div` 2
            bottomPaddingNum = targetHeight - sideHeight - topPaddingNum
        in replicate topPaddingNum paddingLine
           ++ sideLines
           ++ replicate bottomPaddingNum paddingLine
      leftLines = lines left
      rightLines = lines right
      leftLen = maximum $ map length leftLines
      rightLen = maximum $ map length rightLines
      maxHeight = max (length leftLines) (length rightLines)
      lines' = zipWith (++)
                (padLines leftLines leftLen maxHeight)
                (padLines rightLines rightLen maxHeight)
   in unlines lines'