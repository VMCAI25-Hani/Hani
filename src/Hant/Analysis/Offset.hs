{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "[Replace {rtype = Expr, pos = SrcSpan {startLine = 26, startCol = 48, endLine = 26, endCol = 65}, subts = [("x",SrcSpan {startLine = 26, startCol = 49, endLine = 26, endCol = 64})], orig = "x"}]" #-}
module Hant.Analysis.Offset
  ( segmentLength,
    segmentStartIndex,
    segmentEndpoints,
    locationIndices,
    indexTrace,
    oneToEndIdx,
    initialLocationIndex,
    initialSegmentIndex,
  )
where

import Hant.Analysis.Trace (Index)
import Hant.Ast.Diagram (Bound)

segmentLength :: Bound -> Int
segmentLength b = b + 1

-- the index starts from 0
segmentStartIndex :: Bound -> Index -> Int
segmentStartIndex b indexOfSegment = (segmentLength b) * indexOfSegment + 1

segmentEndpoints :: Int -> Bound -> (Int, Int)
segmentEndpoints offset b = (offset + 1, offset + b)

maxLocationIndex :: Bound -> Int -> Int
maxLocationIndex b traceLen = (traceLen + 1) * (segmentLength b)

locationIndices :: Bound -> Int -> [Int]
locationIndices b traceLen = [1 .. maxLocationIndex b traceLen]

indexTrace :: [a] -> [(a, Int)]
indexTrace s = zip s [1 ..]

oneToEndIdx :: Int -> [Int]
oneToEndIdx idx = [1 .. idx]

initialLocationIndex :: Index
initialLocationIndex = 1

initialSegmentIndex :: Index
initialSegmentIndex = 0