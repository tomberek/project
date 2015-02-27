{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Arrows               #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
module Main where
--import Control.Arrow

import           Prelude                   hiding (id, (.))
import           Control.Category
import           Control.Monad.Trans --mtl
import           Control.Monad.State
import           System.IO.Unsafe
import qualified Arrow as A
import Auto
--import           Auto
import Data.List(intercalate)
import Control.Concurrent
import ExampleNeedle
import Control.Arrow
import Control.CCA
import Control.CCA.CCNF
import Control.CCA.Types
import Examples

zero :: Int
zero = 0
{-
summerIO :: AutoX IO Int Int
summerIO = summer

-- creates channels and sparks a thread to fill them
async' :: (b->IO a) -> AutoXIO b (Chan a)
async' recieve = autoIO $ \a -> do
    i <- newChan
    _ <- forkIO $ recieve a >>= writeChan i
    return (Just i,runAutoXIO $ async' recieve)

-- Uses IO to show an async prompt
getText :: AutoXIO String (Chan String)
getText = async' (\b -> print b >> getLine)

-- The two below are different in behavior.
-- getPair is parallel with network calls
-- getPair2 is sequential with network calls
--getPair :: AutoXIO (String,String) (Int,Int)
--getPair = getURLSum *** getURLSum

doTwice :: MonadFix m => AutoX m a b -> AutoX m (a,a) (b,b)
doTwice f = f *** f

summer2 :: AutoX (StateT Int IO) Int Int
summer2 = proc c -> do
    st <- arrM (const get) -< ()
    arrM (modify . (+) ) -< c
    returnA -< st + c

--newArrowChan :: (MonadBaseControl IO m, MonadIO m) => AutoX m b c -> m (Kleisli m b c)
line3 :: Arr f Maybe (Int,Int) Int
line3 = proc (a,b) -> do
    x <- Arr ((+) 2) -< a
    y <- Arr ((+) 1) -< b
    returnA -< x+y

--piTargeter :: Arr (AutoX m) m Double Double
piTargeter :: Monad m => Arr (AutoX m) m Double Double
piTargeter = proc control -> do
    rec let err = control - response
        errSums  <- Effect summer -< err

        input    <- laggingSummer (0::Double)  -< 0.2 * err + 0.01 * errSums
        response <- blackBoxSystem -< input

    id -< response
  where
    blackBoxSystem = id

laggingSummer :: (Monad m,Num a) => a -> Arr (AutoX m) m a a
laggingSummer initial = Effect summer >>> Arrow.init initial
{-
doubleGetter :: Arr (AutoXIO) IO (String,String) (Int,Int)
doubleGetter = proc (a,b) -> do
    z <- getURLSum *** getURLSum -< (a,b)
    returnA -< z

doubleGetter2 = proc (a,b) -> do
    x <- getURLSum -< a
    y <- getURLSum -< b
    returnA -< x+y
---}
---}
d :: A.Arr AutoXIO IO Int Int
d = $(norm testOsc)

e :: A.Arr AutoXIO IO String Int
e = $(norm getURLSum)

f :: A.Arr AutoXIO IO (String,String) (Int,Int)
f = $(norm getURLSum3)

main :: IO ()
main = do
    putStrLn $ show d
    putStrLn $ show e
    putStrLn $ show f
    -- draw $ take 2 $ iterate normalize test
    putStrLn "break"
    --draw $ take 2 $ iterate normalize doubleGetter
    --urlChan <- newArrowChan $ evalA _ doubleGetter
    --out1 <- runKleisli urlChan ("http://example.com","http://google.com")
    --print line2
    --lineChan <- newArrowChan $ line2
    --out1 <- runKleisli lineChan ("http://example.com","http://google.com")
    --print out1
    putStrLn "break"
    --draw $ take 2 $ iterate normalize doubleGetter2
    {-
    print line2
    print line3
    print $ normalize line2
    print $ normalize line3
    --}

draw x = putStrLn $ intercalate "\n\n" $ map show x
-- ***************** HELPING Functions ******
{-
forkArrow :: AutoXIO b c -> ArrowChan IO c b -> IO ThreadId
forkArrow (AutoXIO arrow) (ArrowChan writer reader) =
    let action arrowAction = do
            a <- reader
            (mb,out) <- runAutoX arrowAction a
            case mb of
                Just b -> writer b
                Nothing -> return ()
            action out
    in forkIO $ action arrow

data SChan a = SC (Chan a) (Chan a)
data ArrowChan m a b = ArrowChan (a->m ()) (m b)
instance Functor m => Functor (ArrowChan m a) where
    fmap f (ArrowChan w r) = ArrowChan w (fmap f r)
newArrowChan :: AutoXIO b c -> IO (Kleisli IO b c)
newArrowChan arrow = do
    achan <- liftIO newChan -- IN
    bchan <- liftIO newChan -- OUT
    let writer b = liftIO . writeChan b
        reader   = liftIO . readChan
    _ <- forkArrow arrow $ ArrowChan (writer bchan) (reader achan)
    return $ Kleisli $ \a -> do
        writer achan a
        reader bchan



{-# NOINLINE buffer #-}
buffer :: MVar a
buffer= unsafePerformIO newEmptyMVar
{-# NOINLINE rexit #-}
rexit :: forall a. MVar a
rexit= unsafePerformIO newEmptyMVar
stay :: forall a. IO a
stay= takeMVar rexit

(!>) :: a -> b -> a
(!>) = const .id
infixr 0 !>
---}

