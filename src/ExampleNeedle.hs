{-# LANGUAGE QuasiQuotes #-}
module ExampleNeedle where
import Control.Arrow.Needle
import Auto
import Arrow

test :: Arr (AutoXIO) IO (String,String) (Int,Int)
test = [nd|
    }==={getURLSum}===>
    }==={getURLSum}===>
|]
                                                                     --{arr $ uncurry (+)}===>