{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TemplateHaskell #-}
module Examples where

import Control.CCA.Types
import Control.CCA
import Prelude hiding (init, exp)
import Control.Arrow
import Arrow
import Network.HTTP
import Data.Time
import Control.Concurrent (threadDelay)

dt = (1::Double) / (fromIntegral sr)
sr = 44100 :: Int

getURLSum :: (M a ~ IO, ArrowInit a) => a String Int
getURLSum = (arrM' [| processURL |]  processURL) >>> (arr' [| length |] length)

getURLSum2 :: (M a ~ IO, ArrowInit a) => a (String,String) (Int,Int)
getURLSum2 = getURLSum *** getURLSum

getURLSum3 :: (M a ~ IO, ArrowInit a) => a (String,String) (Int,Int)
getURLSum3 = proc (a,b) -> do
    x <- getURLSum -< a
    y <- getURLSum -< b
    returnA -< (x,y)

processURL :: String -> IO String
processURL a = do
    getCurrentTime >>= print
    threadDelay 1000000
    response <- simpleHTTP (getRequest a)
    getResponseBody response

testOsc :: (M a ~ IO,ArrowInit a) => a Int Int
testOsc = arr' [| \a-> a+1 |] (+1)