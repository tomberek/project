{-# LANGUAGE Arrows #-}
module Examples where

import Control.CCA
import Prelude hiding (init, exp)
import Control.Arrow (returnA)
import Language.Haskell.TH

dt = (1::Double) / (fromIntegral sr)
sr = 44100 :: Int

integral :: ArrowInit a => a Double Double
integral = proc x -> do
  rec let i' = i + x * dt
      i <- init 0 -< i'
  returnA -< i