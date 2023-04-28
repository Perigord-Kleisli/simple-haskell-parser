module Main where

import IndentTests
import Test.Hspec

main :: IO ()
main = do 
  hspec indentSpec
