import Prelude hiding (head, abs)

{-@ abs :: Int -> { n : Int | 0 <= n } @-}
abs :: Int -> Int
abs x = if x < 0 then negate x else x

{-@ head :: { xs : [a] | 1 <= len xs } -> a @-}
head :: [a] -> a
head (x:_) = x

{-@ example :: x : Int -> { y : Int | x == y } @-}
example :: Int -> Int
example x = if abs x < 0 then head [] else x
