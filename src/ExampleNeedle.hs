{-# LANGUAGE Arrows #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
module ExampleNeedle where
--import Auto
--import Arrow
import Control.CCA.Types

import Network.HTTP
import Data.Time
import Control.Arrow
import Control.Concurrent (threadDelay)
import Language.Haskell.TH
{-
test :: Arr (AutoXIO) IO (String,String) (Int,Int)
test = [nd|
    }==={getURLSum}===>
    }==={getURLSum}===>
|]
---}