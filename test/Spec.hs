import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck (arbitrary, Property, quickCheck, (==>))
import Test.QuickCheck.Monadic (assert, monadicIO, pick, pre, run)

import Abstracts
main :: IO ()
main = defaultMain tests

tests = [
    testGroup "Show Output" [
      testProperty (str "readCompose") $ readCompose `expect` "[1,2,3,4]",
      testProperty (str "readApplicative") $ readApplicative `expect` ["2","4","6","8"],
      testProperty (str "readSemigroup") $ readSemigroup `expect` "'S''h''o''r''t'' ''T''e''x''t'' ''L''o''n''g'' ''T''e''x''t'",
      testProperty (str "readRightAssoc") $ readRightAssoc `expect` "'H''e''l''l''o'",
      testProperty (str "readComposeTwoAct") $ readComposeTwoAct `expect` Just "Doing well",
      testProperty (str "readLexScope") $ readLexScope `expect` [6,4,2],
      testProperty (str "readSeqApp") $ readSeqApp `expect` ("hello world!",2017),
      testProperty (str "readSeqApp2") $ readSeqApp2 `expect` "LeftRight"
    ]
  ]

expect :: Eq a => a -> a -> Bool
expect f e = f == e

str :: [Char] -> [Char]
str s = "Return from " ++ s ++ " function"

-- TODO: Test IO
-- readCombineAlls
-- readIO :: Property
-- readIO = monadicIO $ do 
--     assert $ readCombineAlls == readLn ""