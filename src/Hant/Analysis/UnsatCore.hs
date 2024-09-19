module Hant.Analysis.UnsatCore
  ( SmtFormulaTag (..),
    propertiesName,
    initialName,
    segmentName,
    parseUnsatCore,
    pruneTracesViaUnsatCore,
    unsatCoreToFragment
  )
where

import Control.Monad (void)
import Data.List (groupBy, isInfixOf, sortOn)
import Data.Maybe (mapMaybe)
import Data.Text (pack)
import Hant.Analysis.Trace (Index, Trace)
import Hant.Ast.Diagram (Automaton, Name, aname, Message)
import Hant.Util (Parser, symbolS)
import Text.Megaparsec (choice, many, manyTill, parse, try, (<|>))
import Text.Megaparsec qualified as Mega
import Text.Megaparsec.Char (letterChar, numberChar)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Printf (printf)

data SmtFormulaTag
  = Properties
  | Initial Name
  | Segment Name Index
  deriving (Eq, Show)

data AutomatonIndice
  = AutomatonIndice Name [Index]
  deriving (Eq, Show)

data Fragment
  = Fragment Name Index Index
  deriving (Eq, Show)

propertiesName :: String
propertiesName = "$properties"

initialName :: Automaton -> String
initialName = printf "@%s" . aname

segmentName :: Name -> Index -> String
segmentName = printf "%s,%d"

name :: AutomatonIndice -> Name
name (AutomatonIndice n _) = n

smtFormulaTagParser :: Parser SmtFormulaTag
smtFormulaTagParser =
  choice
    [ Properties <$ symbolS (pack propertiesName),
      try initialParser,
      segmentParser
    ]
  where
    initialParser = do
      void (symbolS "@")
      n <- many letterNumber
      return (Initial (pack n))
    segmentParser = do
      n <- manyTill letterNumber (symbolS ",")
      Segment (pack n) <$> decimal
    letterNumber :: Parser Char
    letterNumber = letterChar <|> numberChar

parseFormula :: String -> SmtFormulaTag
parseFormula s = case parse smtFormulaTagParser "" (pack s) of
  Left e -> error ("parse smt formula tag failed: " ++ Mega.errorBundlePretty e)
  Right tag -> tag

parseUnsatCore :: [String] -> [SmtFormulaTag]
parseUnsatCore = fmap parseFormula

filterSegment :: Eq a => [[a]] -> [a] -> [[a]]
filterSegment lists fragment = filter (not . isInfixOf fragment) lists

pruneTracesViaUnsatCore ::
  [Trace] ->
  Trace ->
  [String] ->
  [Trace]
pruneTracesViaUnsatCore traces trace cores =
  filterSegment traces fragment
  where
    fragment = unsatCoreToFragment trace cores

unsatCoreToFragment :: Trace -> [String] -> [Message]
unsatCoreToFragment trace cores = 
  slice (li-1) (ri-1) trace
  where
    formulas = parseUnsatCore cores
    fragments = indicesToFragment <$> groupByAutomaton formulas
    bounds = fragmentToBound <$> fragments
    merge (l, r) (l', r') = (min l l', max r r')
    (li, ri) = foldl merge (maxBound, minBound) bounds

fragmentToBound ::
  Fragment ->
  (Index, Index)
fragmentToBound (Fragment _ l r) = (l, r)

slice :: Int -> Int -> [a] -> [a]
slice from end xs = take (end - from + 1) (drop from xs)

formulaToAutomatonIndice :: SmtFormulaTag -> Maybe AutomatonIndice
formulaToAutomatonIndice (Segment n i) = Just (AutomatonIndice n [i])
formulaToAutomatonIndice (Initial _) = Nothing
formulaToAutomatonIndice Properties = Nothing

groupByAutomaton :: [SmtFormulaTag] -> [AutomatonIndice]
groupByAutomaton =
  fmap mergeAll . groupBy sameAutomaton . sortOn name . mapMaybe formulaToAutomatonIndice
  where
    sameAutomaton (AutomatonIndice n1 _) (AutomatonIndice n2 _) = n1 == n2
    merge (AutomatonIndice n1 is1) (AutomatonIndice n2 is2) = AutomatonIndice (selectName n1 n2) (is1 ++ is2)
    selectName n "" = n
    selectName "" n = n
    selectName n1 n2 = if n1 == n2 
                          then n1 
                          else error "impossible"
    mergeAll = foldl merge (AutomatonIndice "" [])

indicesToFragment :: AutomatonIndice -> Fragment
indicesToFragment (AutomatonIndice n is) = Fragment n (minimum is) (maximum is)
