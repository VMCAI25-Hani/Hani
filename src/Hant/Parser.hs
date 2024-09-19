module Hant.Parser
  ( parseShan,
  )
where

import Hant.Ast.Diagram (Automaton, SequenceDiagram)
import Hant.Ast.Diagram.Parser (parseDiagram)
import Hant.Uxf.Uxf (parseUxfFolder)

parseShan :: FilePath -> IO [Either SequenceDiagram Automaton]
parseShan p =
  parseDiagram <$$> parseUxfFolder p
  where
    (<$$>) = fmap . fmap