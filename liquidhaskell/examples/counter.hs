{-@
test
    :: x : { x : Int | x <= 10 }
    ->     { x : Int | x <  10 }
@-}
test :: Int -> Int
test x = x
