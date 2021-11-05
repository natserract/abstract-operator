module Main where
import Abstracts

main :: IO ()
main = do
  print readCompose
  print readApplicative
  print readSemigroup
  print readRightAssoc
  print readComposeTwoAct
  print readLexScope
