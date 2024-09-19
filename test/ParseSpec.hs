{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant $" #-}

module ParseSpec(
  parseSpec
) where

import Test.Syd (Spec, describe, it)
import Hant.Ast.Diagram.Parser (parseJudgement)

parseSpec :: Spec
parseSpec = do
  describe "judgement parser" $
    it "inq2>=0&&outq2>=0&&q2<=100 parses as a judgement" $ 
      print $ parseJudgement "inq2>=0&&outq2>=0&&q2<=100"