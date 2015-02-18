{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE Arrows #-}
module Main where

import Control.Arrow
import Control.Applicative
import Data.Maybe
import Control.Category
import Control.Concurrent
import System.IO
import Control.Monad.State
import Prelude hiding (id,(.))
import Base
import Auto
import Network.HTTP
zero :: Int
zero = 0

pricer :: AutoX IO (Event Int) Int
pricer = AConsX $ \(Event n v) -> do
    _ <- runA n v
    a <- runA Q zero
    b <- runA P zero
    return (Just $ a*b,pricer) !> "pricer: " ++ show (a,b)

pricer2 :: AutoX IO (Event Int) Int
pricer2 = proc (Event n v) -> do
    (p,q) <- arr (\(n',v') -> if n'==P then (v',0) else (0,v')) -< (n,v)
    a <- summer -< p
    b <- summer -< q
    id -< (a*b) !> "pricer: " ++ show (a,b)

summer3 :: AutoX IO Int Int
summer3 = summer

-- creates channels and sparks a thread to fill them
async :: MonadIO m => (b->IO a) -> AutoX m b (Chan a)
async recieve = AConsX $ \a -> liftIO $ do
    chan <- newChan
    _ <- forkIO $ recieve a >>= writeChan chan
    return (Just chan,async recieve)

-- Uses IO to show an async prompt
getText :: AutoX IO String (Chan String)
getText = async (\b -> print b >> getLine)

-- Works a channel by waiting until it is finished or data is available.
worker :: AutoX IO (Chan a) a
worker = AConsX $ \chan -> do
    r <- readChan chan
    return (Just r,pure r)

-- Same thing as above, using arrow notation
worker1 :: AutoX IO (Chan t) t
worker1 = proc chan -> do
    r <- arrM readChan -< chan
    returnA -< r

-- Uses async to start a new thread
getURLSum :: AutoX IO String Int
getURLSum = proc s -> do
    body <- async (simpleHTTP . getRequest >=> getResponseBody) -< s
    res <- worker -< body
    id -< length res

getPair :: AutoX IO (String,String) (Int,Int)
getPair = getURLSum *** getURLSum

main :: IO ()
main = do
    out <- testAutoM_ pricer2 eventList
    print $ show out

    register Q summer3
    register P summer3
    out2 <- testAutoM_ pricer eventList
    print $ show out2

    out4 <- testAutoM_ getPair [("http://www.google.com","http://example.com")]
    print $ show out4
    threadDelay 1000000

 {-
    runTransient $ do
       liftIO $ putStrLn "MAIN MENU"
       async inputLoop  <|> return ()
       colors <|> app

    stay

colors= do
       option "colors" "choose between three colors"
       r <-  color 1  "red"   <|> color 2 "green" <|> color 3 "blue"
       liftIO $ do
           putStr "Chosen: "
           print r
       where
       color :: Int -> String -> TransientIO String
       color n str= liftIO $ option n str >> return str

app= do
       option "app" "applicative expression that return a counter in 2-tuples every second"
       r <-  (,) <$>  number  <*> number
       liftIO $ putStrLn $ "result=" ++ show r

        where
        number= waitEvents $ do
          threadDelay 1000000
          n <- takeMVar counter
          putMVar counter (n+1)
          return  n
        counter=unsafePerformIO $ newMVar (0 :: Int)

instance Monoid Int where
      mappend= (+)
      mempty= 0
-}