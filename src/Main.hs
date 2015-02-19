{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows               #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Main where
import           Control.Arrow
import           Prelude                   hiding (id, (.))
import           Control.Category
import Control.Applicative
import           Control.Concurrent

import           Control.Monad.Trans --mtl
import           Control.Monad.State
import           Control.Monad.Cont

import           Control.Monad.Parallel    as P
import qualified Data.Map                  as M
import           Data.Maybe
import           Data.Time
import           Network.HTTP
import           System.IO.Unsafe
import Unsafe.Coerce
import Data.Typeable
import Control.Concurrent.Async
import           Auto
import Data.Dynamic
import System.IO

zero :: Int
zero = 0

-- uses global state
pricer :: AutoX IO (Event Int) Int
pricer = AConsX $ \(Event n v) -> do
    _ <- runA n v
    a <- runA Q zero
    b <- runA P zero
    return (Just $ a*b,pricer) !> "pricer: " ++ show (a,b)

-- NO global state
pricer2 :: Monad m => AutoX m (Event Int) Int
pricer2 = proc (Event n v) -> do
    p <- if n==P then id -< v else id -< 0
    q <- if n==Q then id -< v else id -< 0
    a <- summer -< p
    b <- summer -< q
    id -< (a*b) !> "pricer2: " ++ show (a,b)

summerIO :: AutoX IO Int Int
summerIO = summer

-- creates channels and sparks a thread to fill them
async' :: MonadIO m => (b->IO a) -> AutoX m b (Chan a)
async' recieve = AConsX $ \a -> liftIO $ do
    i <- newChan
    _ <- forkIO $ recieve a >>= writeChan i
    return (Just i,async' recieve)

-- Uses IO to show an async prompt
getText :: AutoX IO String (Chan String)
getText = async' (\b -> print b >> getLine)

comp :: AutoX IO String String
comp = getText >>> worker >>> arr reverse

-- Works a channel by waiting until it is finished or data is available.
worker :: AutoX IO (Chan t) t
worker = arrM readChan

processURL :: String -> IO String
processURL a = do
    getCurrentTime >>= print
    threadDelay 1000000
    response <- simpleHTTP (getRequest a)
    getResponseBody response

-- Uses async to start a new thread
getURLSum :: AutoX IO String (Chan String)
getURLSum = async' processURL

-- uses sequence instead of async
getURLSum1 :: AutoX IO String Int
getURLSum1 = arr length <<< arrM processURL

workerURL :: AutoX IO (Chan String) Int
workerURL = arr length <<< worker

-- The two below are different in behavior.
-- getPair is parallel with network calls
-- getPair2 is sequential with network calls
getPair,getPair2 :: AutoX IO (String,String) (Int,Int)
getPair = (getURLSum *** getURLSum) >>> (workerURL *** workerURL)
getPair2 = (getURLSum >>> workerURL) *** (getURLSum >>> workerURL)

doTwice :: Monad m => AutoX m a b -> AutoX m (a,a) (b,b)
doTwice f = f *** f

sequenceW :: MonadParallel m => AutoX m a b -> AutoX m [a] [b]
sequenceW as = AConsX $ \ss -> do
    res <- P.mapM (runAutoX as) ss
    return (Prelude.mapM fst res,sequenceW as)

summer2 :: AutoX (StateT Int IO) Int Int
summer2 = proc c -> do
    st <- arrM (\_ -> get) -< ()
    arrM (\a -> modify ((+) a)) -< c
    returnA -< st + c

{-# NOINLINE buffer #-}
buffer :: MVar a
buffer= unsafePerformIO $ newEmptyMVar
{-# NOINLINE rexit #-}
rexit= unsafePerformIO newEmptyMVar
stay= takeMVar rexit

eventLoop :: AutoX (StateT [Chan a] IO) a b -> [Chan a] -> Chan b -> IO ()
eventLoop stuff inputChan outChan = do
    print $ "entered " ++ (show $ length inputChan)
    input <- readChan $ head inputChan
    ((mb2,out),b1) <- flip runStateT inputChan $ runAutoX stuff input
    putStr "State Length: "
    print $ length b1
    case mb2 of
        Just mb -> print "writing" >> writeChan outChan mb >> print "didit"
        Nothing -> print "Nothing" >> return ()
    putStr " looping"
    eventLoop out inputChan outChan
instance Monad (AutoX (StateT [Chan a] IO) a) where
    return = pure
    mx >>= f = AConsX $ \a -> do
        outchan <- liftIO newChan
        inchan <- liftIO newChan
        chans <- get
        _ <- liftIO $ forkIO $ eventLoop mx chans outchan
        modify $ (:) inchan
        liftIO $ writeChan inchan a
        result <- liftIO $ readChan outchan
        let cont = f result
        runAutoX cont a
---}
loopIt x = x >> loopIt x
main :: IO ()
main = do
    input <- newChan
    writeChan input "help"
    ((Just moutput,_),inputChans::[Chan String]) <- flip runStateT [input] $ runAutoX (do
       arrM (const . liftIO $ putStr "1:" >> hFlush stdout >> getLine >>= print . (++) "1a: " >> hFlush stdout >> return "done1")
       arrM (const . liftIO $ putStr "2:" >> hFlush stdout >> getLine >>= print . (++) "2a: " >> hFlush stdout >> return "done2")
       arrM (const . liftIO $ putStr "3:" >> hFlush stdout >> getLine >>= print . (++) "3a: " >> hFlush stdout >> return "done3")
       arrM (const . liftIO $ print "4:" >> hFlush stdout >> threadDelay (1000 * 100) >> return "done4")
       ) "hi"
    putStr "inputChans length "
    print $ length inputChans
    writeChan input "help"
    --writeChan (inputChans!!3) "go"

    print moutput
    stay
    {--
    out <- testAutoM_ pricer2 eventList
    print $ show out

    register Q summerIO
    register P summerIO
    out2 <- testAutoM_ pricer eventList
    print $ show out2
    stay
    ---}
{--
asyncIO io = AConsX $ \a -> do
    b <- liftIO $ tryTakeMVar buffer
    case b of
        Nothing -> do
            mvar <- liftIO $ newEmptyMVar
            liftIO $ forkIO $ do
                r <- io
                ((Just r2,_),_) <- liftIO $ flip runStateT (_) $ runAutoX (unsafeCoerce _ r) a
                putMVar mvar r2
            --res <- liftIO $ readMVar mvar
            return (Just mvar,asyncIO io) --asyncIO io)
        Just (r,e) -> return (r,e)
        where
            loop io x = io >>= x >> loop io x
---}

    {--
    out3 <- testAutoM_ getPair [("http://www.google.com","http://example.com")]
    print $ show out3
    --out4 <- testAutoM_ getPair2 [("http://www.google.com","http://example.com")]
    --print $ show out4

    (out5,_) <- runAutoX (sequenceW getURLSum1) ["http://www.google.com","http://example.com","http://www.cnn.com"]
    print out5

    (out6,_) <- runAutoX comp "reversing prompt: "
    print out6
    ---}

-- ***************** HELPING Functions ******
(!>) :: a -> b -> a
(!>) = const .id
infixr 0 !>

data EvType = Q | P deriving (Show, Eq, Ord, Enum, Bounded,Read)
data Event a = Event EvType a deriving Show
eventList :: [Event Int]
eventList=[ Event Q 10, Event P 2, Event P 3
            ,Event Q 30, Event P 4]

{-# NOINLINE eventHandlers #-}
eventHandlers :: MVar (M.Map EvType (AutoX m a b))
eventHandlers = unsafePerformIO $ newMVar M.empty

register :: EvType -> AutoX m a b -> IO ()
register name f = do
    evs <- takeMVar eventHandlers
    putMVar eventHandlers $ M.insert name f evs !> "register "++ show name

runEvent :: (MonadIO m) => EvType -> AutoX m a b
runEvent name = AConsX $ \input -> do
    this <- liftIO $ takeMVar eventHandlers
    (res,out) <- case M.lookup name this of
          Just e -> runAutoX e input
          Nothing -> return (Nothing
                            , error "missing handler"
                            )
    liftIO $ putMVar eventHandlers $ M.insert name out this !> "put back in"
    return (res,out)

runA :: EvType -> a -> IO b
runA event value = liftM (fromJust . fst) $ runAutoX (runEvent event) value

