{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent.STM (STM)
import Control.Monad (forever)
import Control.Monad.Managed (Managed, liftIO)
import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))

import qualified Control.Concurrent             as Concurrent
import qualified Control.Concurrent.Async       as Async
import qualified Control.Concurrent.STM         as STM
import qualified Control.Concurrent.STM.TBQueue as TBQueue
import qualified Control.Monad.Managed          as Managed
import qualified Network.HTTP.Types             as HTTP
import qualified Network.Wai                    as Wai
import qualified Network.Wai.Handler.Warp       as Warp
import qualified System.IO                      as IO

newtype Transaction a = Transaction { getTransaction :: STM a }

instance Monoid (Transaction a) where
    mempty = Transaction STM.retry

    mappend (Transaction l) (Transaction r) = Transaction (l `STM.orElse` r)

data Event = Tick | KeyPress Char | Message ByteString deriving (Show)

chars :: Managed (Transaction Event)
chars = do
    queue <- liftIO (TBQueue.newTBQueueIO 100)
    let thread = forever (do
            char <- getChar
            STM.atomically (TBQueue.writeTBQueue queue (KeyPress char)) )
    _ <- Managed.managed (Async.withAsync thread)
    return (Transaction (TBQueue.readTBQueue queue))

ticks :: Managed (Transaction Event)
ticks = do
    queue <- liftIO (TBQueue.newTBQueueIO 100)
    let thread = forever (do
            Concurrent.threadDelay 1000000
            STM.atomically (TBQueue.writeTBQueue queue Tick) )
    _ <- Managed.managed (Async.withAsync thread)
    return (Transaction (TBQueue.readTBQueue queue))

messages :: Managed (Transaction Event)
messages = do
    queue <- liftIO (TBQueue.newTBQueueIO 100)
    let application request respond = do
            bytestring <- Wai.strictRequestBody request
            STM.atomically (TBQueue.writeTBQueue queue (Message bytestring))
            respond (Wai.responseBuilder HTTP.status200 [] "")
    let thread = Warp.run 8080 application
    _ <- Managed.managed (Async.withAsync thread)
    return (Transaction (TBQueue.readTBQueue queue))

events :: Managed (Transaction Event)
events = chars <> ticks <> messages

main :: IO ()
main = do
    IO.hSetBuffering IO.stdin IO.NoBuffering
    IO.hSetEcho      IO.stdin False
    Managed.runManaged (do
        transaction <- events
        liftIO (forever (do
            event <- STM.atomically (getTransaction transaction)
            print event ) ))
