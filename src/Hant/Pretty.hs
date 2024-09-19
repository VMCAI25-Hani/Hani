module Hant.Pretty
  ( banner,
    line,
    separationLine,
    blank
  )
where

banner :: String -> IO ()
banner info = do
  let l = length info
  line l
  putStrLn info
  line l

line :: Int -> IO ()
line n = putStrLn $ replicate n '-'

separationLine :: IO ()
separationLine = putStrLn $ replicate 35 '-'

blank :: IO ()
blank = putStr $ replicate 2 '\n'