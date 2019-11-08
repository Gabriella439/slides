import Data.Set ()

{-@ 
delete :: Eq a => x : a -> [a] -> { zs : [a] | not (Set_mem x (listElts zs)) }
@-}
delete :: Eq a => a -> [a] -> [a]
delete _  []    = []
delete x (y:ys)
    | x == y    =   delete x ys
    | otherwise = y:delete x ys
