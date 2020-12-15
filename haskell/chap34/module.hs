module Main where

main :: IO()
main = return()

-- Preludeで定義されているheadの再定義、上のmodule文がないとエラーになる
head :: Monoid a => [a] -> a
head (x:xs) = x
head [] = mempty

example :: [[Int]]
example = []

-- Main.head example => []
-- Prelude.head example => Exception: Prelude.head empty List

--  クイックチェック1
length :: Int
length = 8
-- Main.length => 8
