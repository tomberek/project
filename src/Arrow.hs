{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
-- originally from http://stackoverflow.com/questions/12001350/useful-operations-on-free-arrows
-- then for inclusion in Control.Arrow.Free
module Arrow where
import Control.Arrow
import Control.Category
import Prelude hiding ((.), id)
import Data.Typeable
import Control.Monad((>=>))
import Control.Monad.Fix
import qualified Debug.Trace as T
import Control.CCA.Types

type IOaction a b = a -> IO b
-- | The free 'Arrow' for a 'Functor' @f@
data Arr f m a b where
    Id2 :: Arr f m a a
    Swap :: Arr f m (a,b) (b,a)
    Pierce :: Arr f m (a,b) ((a,()),b)
    Fst :: Arr f m (a,b) a
    Raise :: Arr f m a (a,())
    Arr :: (a -> b) -> Arr f m a b
    ArrM :: (a -> m b) -> Arr f m a b
    First :: Arr f m a b -> Arr f m (a, d) (b, d)
    Second :: Arr f m a b -> Arr f m (d, a) (d, b)
    Effect :: f a b -> Arr f m a b
    (:***) :: Arr f m a b -> Arr f m c d -> Arr f m (a,c) (b,d)
    (:>>>) :: Arr f m a c -> Arr f m c b -> Arr f m a b
    (:&&&) :: Arr f m a b -> Arr f m a b' -> Arr f m a (b,b')
    Loop :: Arr f m (a, d) (b, d) -> Arr f m a b
    LoopD :: e -> ((a, e) -> (b, e)) -> Arr f m a b
    Init :: b -> Arr f m b b
    Fan :: Arr f m a b -> Arr f m a c -> Arr f m a (b,c)
--"andtojoin" forall f g. ((arr ( \(a,_) -> a)) >>> f) &&& ((arr ( \(_,a)->a)) >>> g) = T.trace "fired" $ f *** g
{-# RULES
"id"    forall (f::forall a. a->a).    arr2 f = T.trace "pierce" $ Id2
"pierce"forall (f::(a,b)->((a,()),b)). arr2 f = T.trace "pierce" $ Pierce
"swap"  forall (f::(a,b)->(b,a)).      arr2 f = T.trace "swap" $ Swap
"fst"   forall (f::(a,b)->a).          arr2 f = T.trace "fst" $ Fst
"raise" forall (f::a->(a,())).         arr2 f = T.trace "raise" $ Raise
 #-}
instance Show (Arr f m a b) where
    show (Arr _) = "Arr"
    show Raise = "Raise"
    show Swap = "Swap"
    show Fst = "Fst"
    show Pierce = "Pierce"
    show Id2 = "Id2"
    show (ArrM _) = "ArrM"
    show (First f) = "First " ++ show f
    show (Second f) = "Second " ++ show f
    show (Effect _) = "Effect"
    show (f :>>> g) = "(" ++ show f ++ " >>> " ++ show g ++ ")"
    show (f :*** g) = "[" ++ show f ++ " *** " ++ show g ++ "]"
    show (Loop f) = "Loop " ++ show f
    show (LoopD _ _) = "LoopD"
    show (Init _) = "Init"
    show (Fan f g) = "<" ++ show f ++ " &&& " ++ show g ++ ">"

type Traversal f m = forall a b. Arr f m a b -> Arr f m a b
imap :: Traversal f m -> Traversal f m
imap t (First f) = First (t f)
--imap t (Arr f :>>> (Arr g :>>> h)) = imap t $ Arr (g.f) :>>> t h
--imap t (Arr f :>>> (g :>>> h)) = imap t $ (Arr f :>>> g) :>>> t h
imap t (f :>>> g) = t f :>>> t g
imap t (f :*** g) = t f :*** t g
imap t (Fan f g) = Fan (t f) (t g)
imap t (Loop f) = Loop (t f)
--imap h (Lft
imap t x = x


norm :: (Functor m,MonadFix m) => Arr f m a b -> Arr f m a b
{-
--norm (Arr f :>>> (Arr g :>>> h)) = Arr (g.f) :>>> h
--norm (ArrM f :>>> ArrM g) = ArrM (f >=> g)
norm (First (ArrM f)) = ArrM $ \(a,b) -> do
      a' <- f a
      return (a',b)
--norm (ArrM f :>>> LoopD i g) = LoopD i (g . (f `cross` id))  TODO
norm (Loop (ArrM f)) = ArrM $ \x -> do
    rec (y,z) <- f (x,z)
    return y
---}
norm (Arr f :>>> Arr g) = Arr (g . f)           -- original and below
norm (First (Arr f)) = Arr (f `cross` id)
norm (Arr f :>>> LoopD i g) = LoopD i (g . (f `cross` id))
norm (LoopD i f :>>> Arr g) = LoopD i ((g `cross` id) . f)
norm (LoopD i f :>>> LoopD j g) = LoopD (i,j) (assoc' (juggle' (g `cross` id) . (f `cross` id)))
norm (Loop (LoopD i f)) = LoopD i (trace (juggle' f))
norm (First (LoopD i f)) = LoopD i (juggle' (f `cross` id))
norm (Loop (Arr f)) = Arr (trace f)
norm (Init i) = LoopD i swap
--norm (f :>>> g) = f :>>> norm g --added by Tom
norm e = e
--normalize ArrowChoice?

everywhere :: Traversal f m -> Traversal f m
everywhere h =h. imap (everywhere h)

normalize :: (MonadFix m,Functor m) => Arr f m a b -> Arr f m a b
normalize = everywhere norm

swap (x,y) = (y,x)
cross f g (a,b) = (f a,g b)
assoc ((x,y),z) = (x,(y,z))
assoc' f = assoc . f . unassoc
unassoc (x,(y,z)) = ((x,y),z)
juggle ((x,y),z) = ((x,z),y)
juggle' f = juggle . f . juggle
trace f x = let (y,z) = f (x,z) in y

evalA exec = go
  where
    go freeA = case freeA of
        Arr f -> return $ arr f
        ArrM f -> arr f
        --f1 :*** f2 -> go f1 *** go f2
        --LoopD f1 f2 -> undefined
        Effect eff -> return $ exec eff

effect :: eff a b -> Arr eff m a b
effect = Effect

instance Category (Arr eff m) where
    id = arr2 id
    (.) = flip (:>>>)

{-# NOINLINE arr2 #-}
arr2 :: (a->b) -> Arr f m a b
arr2 = Arr
instance Arrow (Arr eff m) where
    arr = arr2
    first = First
    second = Second
    (***) = (:***)
    (&&&) = Fan
instance ArrowLoop (Arr eff m) where
    loop = Loop
instance ArrowInit (Arr eff m) where
    type M (Arr eff m) = m
    arrM'' = ArrM
    init = Init
{--
analyze :: forall f eff a0 b0 r. (Applicative f, Monoid r)
        => (forall a b. eff a b -> f r)
        -> Arr eff a0 b0 -> f r
analyze visit = go
  where
    go :: forall a b. Arr eff a b -> f r
    go arrow = case arrow of
        Pure _ -> pure mempty
        Seq f1 f2 -> mappend <$> go f1 <*> go f2
        Par f1 f2 -> mappend <$> go f1 <*> go f2
        Fan f1 f2 -> mappend <$> go f1 <*> go f2
        Effect eff -> visit eff


---}