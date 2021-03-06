{-# LANGUAGE TemplateHaskell, FlexibleInstances #-}

module Control.CCA 
  ((>>>), (<<<), first, second, (***), (&&&), loop, 
   Arrow, ArrowLoop, ArrowInit, 
   arr, init, arr', arrM, arrM', init', constant,
   norm, normOpt) where

import Control.Arrow hiding (arr, returnA)
import Control.CCA.Types hiding (init,arrM)
import Control.CCA.CCNF
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Instances
import Prelude hiding (init)

arr :: ExpQ -> ExpQ
arr e = appE [|arr' e|] e

arrM :: ExpQ -> ExpQ
arrM e = appE [|arrM' e|] e

init :: ExpQ -> ExpQ
init i = appE [|init' i|] i

constant :: (ArrowInit a, Lift b) => b -> a () b
constant c = arr' [|const c|] (const c)

