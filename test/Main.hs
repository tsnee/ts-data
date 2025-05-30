module Main (main) where

import Prelude

assertEqual :: (Eq a, Show a) => String -> a -> a -> IO ()
assertEqual label expected actual =
  if expected == actual
    then putStrLn $ "✔ " ++ label
    else error $ "✘ " ++ label ++ ": expected " ++ show expected ++ ", got " ++ show actual

main :: IO ()
main = print "No tests yet"
