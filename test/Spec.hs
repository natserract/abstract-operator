import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

import Abstracts

main :: IO ()
main = defaultMain tests

tests = [
    testGroup "Show Output" [
      testProperty (str "readCompose") $ readCompose `expect` "[1,2,3,4]"
    ]
  ]

expect :: Eq a => a -> a -> Bool
expect f e = f == e

str :: [Char] -> [Char]
str s = "Return from " ++ s ++ " function"