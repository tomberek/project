{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE Arrows #-}
module Main where

import Control.Arrow
import Control.Applicative
import Data.Maybe
import Data.Monoid
import Control.Category
import Control.Concurrent
import System.IO.Unsafe
import System.IO
import Control.Monad.State
import Prelude hiding (id,(.))
import Base
import Auto
import qualified Data.Map as M
import Control.Concurrent.Chan
import Network.HTTP

testAutoX :: Monad m => AutoX m r a -> [r] -> m [Maybe a]
testAutoX ra [] = return []
testAutoX ra (a:as) = do
    (res,out) <- runAutoX ra a
    ress <- testAutoX out as
    return $ res:ress

summer2 :: AutoX (StateT Int IO) Int Int
summer2 = proc c -> do
    state <- arrM (\_ -> get) -< ()
    arrM (\a -> modify ((+) a)) -< c
    returnA -< state + c

simple :: AutoX IO Int Int
simple = AConsX $ \input -> return (Just input,simple) !> "saw: " ++ show input

pricer :: AutoX (IO) (Event Int) Int
pricer = AConsX $ \(Event n v) -> do
    q <- runA n v
    a <- runA Q zero
    b <- runA P zero
    return (Just $ a*b,pricer) !> "pricer: " ++ show (a,b)

zero :: Int
zero = 0
pricer2 :: AutoX IO (Event Int) Int
pricer2 = proc (Event n v) -> do
    (p,q) <- arr (\(n',v') -> if n'==P then (v',0) else (0,v')) -< (n,v)
    a <- summer -< p
    b <- summer -< q
    id -< (a*b) !> "pricer: " ++ show (a,b)

summer3 :: AutoX IO Int Int
summer3 = summer

async :: MonadIO m => IO a -> AutoX m b (Chan a)
async recieve = AConsX $ \a -> liftIO $ do
    chan <- newChan
    forkIO $ recieve >>= writeChan chan
    return (Just chan,async recieve)

asyncProcess :: MonadIO m => (b -> a) -> AutoX m (Chan b) a
asyncProcess f = AConsX $ \chan -> liftIO $ do
    value <- readChan chan
    return (Just  $ f value,pure $ f value)

asyncProduce :: MonadIO m => AutoX m a (Chan a)
asyncProduce = AConsX $ \a -> liftIO $ do
    chan <- newChan
    forkIO $ writeChan chan a
    return (Just chan,asyncProduce)

getText :: AutoX IO String (Chan String)
getText = async getLine

getHTTP = simpleHTTP . getRequest

worker :: AutoX IO (Chan String) Int
worker = AConsX $ \chan -> do
    r <- readChan chan
    res <- getHTTP r
    body <- getResponseBody res
    let len = length . words $ body
    return (Just len,pure len)

sumIt :: AutoX IO (Chan String) ()
sumIt = proc a -> do
      r <- worker -< a
      arrM (\b -> putStrLn $ "results=" ++ show b) -< r

getURLSum :: AutoX IO String ()
getURLSum = proc s -> do
    url <- asyncProduce -< s
    res <- sumIt -< url
    id -< res

main = do
    out <- testAutoM_ (pricer2) eventList
    putStrLn $ show out

    register Q $ summer3
    register P $ summer3
    out2 <- testAutoM_ pricer eventList
    putStrLn $ show out2

    out4 <- testAutoM_ getURLSum ["http://www.google.com","http://example.com"]
    putStrLn $ show out4

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