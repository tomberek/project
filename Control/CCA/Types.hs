{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module Control.CCA.Types where

#if __GLASGOW_HASKELL__ >= 610

import Control.Category

#endif

import Control.Arrow
import Language.Haskell.TH
import Prelude hiding (init)

class (Arrow a, ArrowLoop a) => ArrowInit a where
  type M a :: * -> *
  init :: b -> a b b
  arr' :: ExpQ -> (b -> c) -> a b c
  arr' _ = arr
  arrM' :: ExpQ -> (b -> (M a) c) -> a b c
  arrM' _ = arrM''
  arrM'' :: (b -> (M a) c) -> a b c
  init' :: ExpQ -> b -> a b b
  init' _ = init
  loopD :: e -> ((b, e) -> (c, e)) -> a b c
  loopD i f = loop (arr f >>> second (init i))


