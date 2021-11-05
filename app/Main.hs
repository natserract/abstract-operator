module Main where
import Abstracts
    ( readApplicative,
      readCompose,
      readComposeTwoAct,
      readLexScope,
      readRightAssoc,
      readSemigroup )

main :: IO ()
main = do
  print readCompose
  print readApplicative
  print readSemigroup
  print readRightAssoc
  print readComposeTwoAct
  print readLexScope
