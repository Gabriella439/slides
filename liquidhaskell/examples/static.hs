{-# LANGUAGE OverloadedStrings #-}

import Data.ByteString.Char8 (ByteString)
import Data.Word (Word8)

import qualified Data.ByteString as ByteString

{-@ assume ByteString.pack :: [Char] -> { bs : ByteString | bslen bs == len w8s } @-}

{-@ ByteString.head :: { bs : ByteString | 1 <= bslen bs } -> Char @-}

staticCheck :: Char
staticCheck = head "Hello, world!"
