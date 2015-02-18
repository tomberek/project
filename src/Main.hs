{-# LANGUAGE Arrows               #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           Auto
import           Control.Applicative
import           Control.Arrow
import           Control.Category
import           Control.Concurrent
import           Control.Monad          (liftM)
import           Control.Monad.IO.Class
import           Control.Monad.Parallel as P
import           Control.Monad.State
import qualified Data.Map               as M
import           Data.Maybe
import           Data.Time
import           Network.HTTP
import           Prelude                hiding (id, (.))
import           System.IO
import           System.IO.Unsafe

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
    p <- if n==P then id -< v else id -< 0
    q <- if n==Q then id -< v else id -< 0
    a <- summer -< p
    b <- summer -< q
    id -< (a*b) !> "pricer2: " ++ show (a,b)

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
getURLSum = async processURL

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
    return (Prelude.sequence $ map fst res,sequenceW as)

main :: IO ()
main = do
    out <- testAutoM_ pricer2 eventList
    print $ show out

    register Q summer3
    register P summer3
    out2 <- testAutoM_ pricer eventList
    print $ show out2

    out3 <- testAutoM_ getPair [("http://www.google.com","http://example.com")]
    print $ show out3
    --out4 <- testAutoM_ getPair2 [("http://www.google.com","http://example.com")]
    --print $ show out4

    (out5,_) <- runAutoX (sequenceW $ getURLSum1) ["http://www.google.com","http://example.com","http://www.cnn.com"]
    print out5

    (out6,_) <- runAutoX comp "reversing prompt: "
    print out6

-- ***************** HELPING Functions
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

