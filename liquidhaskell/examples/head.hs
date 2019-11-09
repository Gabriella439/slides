import Prelude hiding (head)

{-@ head :: { xs : [a] | 1 <= len xs } -> a @-}
head :: [a] -> a
head (x:_) = x

example :: Int
example = head []
