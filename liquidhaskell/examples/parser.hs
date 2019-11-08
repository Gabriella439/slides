{-# LANGUAGE RecordWildCards #-}

module Parser where

import Prelude hiding (take)
import Data.ByteString (ByteString)

import qualified Data.ByteString        as ByteString
import qualified Data.ByteString.Unsafe as Unsafe

data Parser a = Parser { runParser :: ByteString -> Maybe (a, ByteString) }

{-@
take :: n : { n : Int | 0 <= n } -> Parser { bs : ByteString | bslen bs == n }
@-}
take :: Int -> Parser ByteString
take n = Parser (\bs ->
    if ByteString.length bs < n
    then Nothing
    else Just (ByteString.splitAt n bs) )

instance Functor Parser where
    fmap f p = Parser (\bs -> fmap adapt (runParser p bs))
      where
        adapt (x, bs) = (f x, bs)

instance Applicative Parser where
    pure x = Parser (\bs -> Just (x, bs))

    mf <*> mx = Parser (\bs0 -> do
        (f, bs1) <- runParser mf bs0
        let adapt (x, bs2) = (f x, bs2)
        fmap adapt (runParser mx bs1) )

instance Monad Parser where
    return x = Parser (\bs -> Just (x, bs))

    m >>= f = Parser (\bs0 -> do
        (x, bs1) <- runParser m bs0
        runParser (f x) bs1 )

{-@
data UDP = UDP
    { udpSourcePort      :: { bs : ByteString | bslen bs == 2 }
    , udpDestinationPort :: { bs : ByteString | bslen bs == 2 }
    , udpLength          :: { bs : ByteString | bslen bs == 2 }
    , udpChecksum        :: { bs : ByteString | bslen bs == 2 }
    }
@-}
data UDP = UDP
    { udpSourcePort      :: !ByteString
    , udpDestinationPort :: !ByteString
    , udpLength          :: !ByteString
    , udpChecksum        :: !ByteString
    }

udp :: Parser UDP
udp = do
    udpSourcePort      <- take 2
    udpDestinationPort <- take 2
    udpLength          <- take 2
    udpChecksum        <- take 2
    return (UDP {..})

udp2 :: Parser UDP
udp2 = Parser (\bs0 ->
    if ByteString.length bs0 < 8
    then Nothing
    else do
        let (udpSourcePort     , bs1) = unsafeSplitAt 2 bs0
        let (udpDestinationPort, bs2) = unsafeSplitAt 2 bs1
        let (udpLength         , bs3) = unsafeSplitAt 2 bs2
        let (udpChecksum       , bs4) = unsafeSplitAt 2 bs3
        Just (UDP {..}, bs4) )

{-@
unsafeSplitAt
    ::  n : { n : Int | 0 <= n }
    ->  i : { bs : ByteString | n <= bslen bs }
    ->  ( { bs : ByteString | bslen bs == n }
        , { bs : ByteString | bslen bs == bslen i - n }
        )
@-}
unsafeSplitAt :: Int -> ByteString -> (ByteString, ByteString)
unsafeSplitAt n bs =
    (Unsafe.unsafeTake n bs, Unsafe.unsafeDrop n bs)

{-@
ByteString.length :: bs : ByteString -> { n : Int | n == bslen bs }
@-}

{-@
ByteString.splitAt
    :: n : Int
    -> i : ByteString
    -> ( { l : ByteString | (n <= 0 ==> bslen i == 0) &&
                            ((0 <= n && n <= bslen i) ==> bslen l == n) &&
                            (bslen i <= n ==> bslen l == bslen i)
         }
       , { r : ByteString | (n <= 0 ==> bslen r == bslen i) &&
                            ((0 <= n && n <= bslen i) ==> bslen r == bslen i - n) &&
                            (bslen i <= n ==> bslen r == 0)
         }
       )
@-}

{-@
Unsafe.unsafeTake
    :: n : { n : Int | 0 <= n }
    -> { i : ByteString | n <= bslen i }
    -> { o : ByteString | bslen o == n }
@-}

{-@
Unsafe.unsafeDrop
    :: n : { n : Int | 0 <= n }
    -> i : { i : ByteString | n <= bslen i }
    -> { o : ByteString | bslen o == bslen i - n }
@-}
