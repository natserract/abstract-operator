-- common abstract operator in haskell
{-# LANGUAGE FlexibleContexts #-}

module Abstracts (
    readCompose
  , readApplicative
  , readSemigroup
  , readRightAssoc
  , readComposeTwoAct
  , readLexScope
  , readSeqApp
  , readSeqApp2
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

-- | Sequential application
-- Sekuensial: sederetan instruksi atau aksi yang akan di eksekusi 
-- A few functors support an implementation of <*> 
-- that is more efficient than the default one.
-- do f <- fs
   -- a <- as
   -- pure (f a)
-- f (a -> b) -> f a -> f b
readSeqApp :: ([Char], Integer)
readSeqApp = ("hello ", (+ 15)) <*> ("world!", 2002)

readSeqApp2 :: [Char]
readSeqApp2 = (++) <*> const "Right" $ "Left"
