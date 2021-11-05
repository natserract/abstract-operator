-- common abstract operator in haskell
{-# LANGUAGE FlexibleContexts #-}

module Abstracts (
    readCompose
  , readApplicative
  , readSemigroup
  , readRightAssoc
  , readComposeTwoAct
  , readLexScope
) where

-- | Application composition
-- ($)  :: (a -> b) ->  a -> b
-- Application composition: function return b
readCompose :: String
readCompose = show $ [1, 2, 3, 4]

-- | Applicative, Functor
-- (<$>) :: Functor f => (a -> b) -> f a -> f b
-- Functor f => (a -> b) -> f a -> f b
-- Functor: Passing value to function parameter
-- Array merupakan turunan dari sebuah functor
readApplicative :: [String]
readApplicative = show . (* 2) <$> [1, 2, 3, 4]

-- | Semi Group
-- An associative operation
-- (<>) :: a -> a -> a
--
-- Different with ++, <> can compose,
-- ++: concat two values with type [a] -> [a] -> [a]
-- <>: grouping two operations with more flexible type a -> a -> a

-- Example:
-- f = (\x -> x ++ "O") ++ (\y -> y++"red") Nope!
-- f' = (\x -> x ++ "O") <> (\y -> y++"red") Ok!
(<<>>) :: Semigroup a => a -> a -> a
(<<>>) s l = s <> l

readSemigroup :: [Char]
readSemigroup = (<<>>) "Short Text" " Long Text" >>= show

-- | Right Association
-- Sequentially compose two actions, passing any value
-- produced by the first as an argument to the second.
--
-- Return operasi pertama, dijadikan argumen pertama pada argumen kedua
readRightAssoc :: [Char]
readRightAssoc = "Hello" >>= show

-- | Compose two action, with discarding first value
-- Sequentially compose two actions, discarding any value produced by the first,
-- like sequencing operators (such as the semicolon) in imperative languages.
--
-- Menggabungkan 2 operasi secara berurutan, tanpa mengambil nilai dari argumen pertama,
-- Operasi kedua berjalan sesuai dengan jumlah operasi pertama
-- Like: if (first operation == 'done') return "Doing well"
readComposeTwoAct :: Maybe [Char]
readComposeTwoAct = pure (Just 1) 0 >> return "Doing well"

-- | Arrow left a.ka. Lexical Scope
(*|) :: Monad m => (t -> m b) -> m t -> m b
(*|) ƒ1 ƒ2 = do
  a <- ƒ2 
  ƒ1 a
  
readLexScope :: [Integer]
readLexScope = (*|) pure . reverse $ map (* 2) [1, 2, 3]

-- readLexScope = show . showDoAct (map (\x -> x * 2))

-- go = map (\x -> x) [2, 3]

-- arrow' :: (Show a , Num a) => [a] -> [Char]
-- arrow' [] = []
-- arrow' xs = do
--   days <- map (* 2) xs
--   daysR <- reverse xs
--   show [days, daysR]
-- >>= = do a <- as
--        bs a

-- fo x y = pure 2

-- readFo = fo [2, 3] [2, 3]

-- arrowNext' fs as = do
--   f <- fs
--   f <$> as

-- showApplFunctor = pure
-- showApplFunctor fs as = do
--     f <- fs
--     f <$> as

-- print $ readSemigroup "Short"
-- print showApplFunctor
-- print $ arrow' [2, 3]
-- print . show $ "12" >> readFo
-- print . show $ arrow' [2, 3]
-- putStrLn "Hello What is your name?"
--         >> getLine >>= \name -> putStr ("Hello" ++ name ++ "!")
