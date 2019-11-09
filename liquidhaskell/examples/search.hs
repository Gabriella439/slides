import Data.Vector (Vector)

import qualified Data.Vector

{-@
search
    :: Ord a
    => element : a
    -> vector : Vector a
    -> minIndex : { minIndex : Int | 0  <= minIndex && minIndex <  vlen vector }
    -> maxIndex : { maxIndex : Int | minIndex <  maxIndex && maxIndex <= vlen vector }
    ->            {    index : Int | minIndex <= index && index < maxIndex }
    /  [maxIndex - minIndex]
@-}
search :: Ord a => a -> Vector a -> Int -> Int -> Int
search element vector minIndex maxIndex
    | minIndex + 1 == maxIndex = minIndex
    | element < testElement    = search element vector minIndex testIndex
    | otherwise                = search element vector testIndex maxIndex
  where
    testIndex = (minIndex + maxIndex) `div` 2

    testElement = Data.Vector.unsafeIndex vector testIndex
