import Prelude hiding (splitAt)

{-@
splitAt
    :: n : { n : Int | 0 <= n }
    -> xs : { xs : [a] | n <= len xs }
    -> ({ ls : [a] | len ls == n }, { rs : [a] | len rs = len xs - n })
@-}
splitAt :: Int -> [a] -> ([a], [a])
splitAt 0  xs    = (  [], xs)
splitAt n (x:xs) = (x:ls, rs)
  where
    ~(ls, rs) = splitAt (n - 1) xs
