module Hant.Analysis.LocMap
  ( LocMap,
    ReverseMap,
    constructMap,
    reverseMap,
    llookup,
    rlookup,
  )
where

import Data.Map (Map)
import Data.Map qualified as M
import Data.Text (Text)
import Data.Word (Word8)
import Hant.Ast.Diagram (Automaton (Automaton), Node (..))

type LocMap = Map (Text, Text) Word8

type ReverseMap = Map (Text, Word8) Text

constructMap :: [Automaton] -> LocMap
constructMap ms =
  M.fromList $ concatMap entries ms
  where
    entries (Automaton n _ nodes _ _) = entry n <$> zip nodes [0 ..]
    entry n (Node _ nn _ _ _, i) = ((n, nn), i)

llookup :: LocMap -> (Text, Text) -> Word8
llookup lmap key = case M.lookup key lmap of
  Just v -> v
  Nothing -> error ("impossible " ++ show key ++ "\n" ++ show lmap)

reverseMap :: LocMap -> ReverseMap
reverseMap = M.fromList . fmap (\((n, nn), v) -> ((n, v), nn)) . M.toList

rlookup :: ReverseMap -> (Text, Word8) -> Text
rlookup rmap key = case M.lookup key rmap of
  Just v -> v
  Nothing -> error ("impossible " ++ show key ++ "\n" ++ show rmap)