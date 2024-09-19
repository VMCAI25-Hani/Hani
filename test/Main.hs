module Main (main) where

import Test.Syd (sydTest)
import ParseSpec (parseSpec)

{-# OPTIONS_GHC -F -pgmF sydtest-discover #-}
main :: IO ()
main = sydTest parseSpec