{-# LANGUAGE Arrows #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module ExampleNeedle where
import Control.Arrow
import Auto
import Arrow
import Control.CCA
{-
test :: Arr (AutoXIO) IO (String,String) (Int,Int)
test = [nd|
    }==={getURLSum}===>
    }==={getURLSum}===>
|]
---}
--test :: ArrowInit a -> a (String,String) (Int,Int)
test = proc (a,b) -> do
    x <- getURLSum -< a
    y <- getURLSum -< b
    returnA -< (x,y)