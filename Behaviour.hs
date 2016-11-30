{-# Language NoMonomorphismRestriction, BangPatterns, RecursiveDo, TupleSections, TemplateHaskell, ScopedTypeVariables, InstanceSigs #-}
module Behaviour where
import Control.Monad
import Reactive.Banana hiding (stepper, (<@), Behavior)
import Reactive.Banana.Frameworks 
import DebugTH
import Debug.Trace
import Control.Arrow

newtype Behaviour a = B (MomentIO (a, Event a))
instance Functor Behaviour where
  fmap f (B m) = B (fmap (\(a, e) -> (f a, fmap f e)) m)
instance Applicative Behaviour where
  pure a = B (return (a, never))
  (<*>)::forall a b.Behaviour ( a -> b) -> Behaviour a -> Behaviour b
  (B mf) <*> (B ma) = B $ do
      (f, ef) <- mf
      (a, af) <- ma
      let efFunc = fmap (\x (_, y) -> (x,y)) (ef :: Event (a -> b))
          afFunc = fmap (\y (x, _) -> (x,y)) af
      --stream :: Event (a -> b, a)
      stream <- accumE (f, a) (unionWith (.) efFunc afFunc)
      return (f a, (fmap (uncurry ($)) stream))
-- instance Monad Behaviour where
--   return = pure
--   (B ma) >>= (B mf) = do
--       (a, af) <- ma
--       (f, ff) <- mf
--       liftM2 (,) (f a) (ff af)
  -- (<*>)::forall a b.Behaviour ( a -> b) -> Behaviour a -> Behaviour b
  -- (B mf) <*> (B ma) = B $ do
  --     (f, ef) <- mf
  --     (a, af) <- ma
  --     let efFunc = fmap (\x (_, y) -> (x,y)) (ef :: Event (a -> b))
  --         afFunc = fmap (\y (x, _) -> (x,y)) af
  --     --stream :: Event (a -> b, a)
  --     stream <- accumE (f, a) (unionWith (.) efFunc afFunc)
  --     return (f a, (fmap (uncurry ($)) stream))

unionsC :: [Event a] -> Event a
unionsC = foldr (unionWith const) never

unionWithP :: (a -> b -> c) -> a -> b -> Event a -> Event b -> MomentIO (Event c)
unionWithP f inita initb a b = toEvent $ liftA2 f (stepper inita a) (stepper initb b)

stepper :: a -> Event a -> Behaviour a
stepper a e = B $ return (a, e)

toEvent :: Behaviour a -> MomentIO (Event a)
toEvent (B m) = fmap snd m

accumB a e = stepper a <$> accumE a e

(<@) :: Behaviour a -> Event b -> MomentIO (Event a)
(B m) <@ e = do
      (a, af) <- m
      let aFunc = fmap (\x _ -> (x, False)) af
          eFunc = fmap (\_ (x, _) -> (x, True)) e
      --stream :: Event (a -> b, a)
      stream <- accumE (a, False) (unionWith (\_ g p -> (fst (g p), True)) eFunc aFunc)
      return $ fmap fst $ filterE (\(_, b) -> b ) stream
      --return (f a, (fmap (uncurry ($)) stream))

(<@>) :: Behaviour (a -> b) -> Event a -> MomentIO (Event b)
b <@> e  = (liftA2 ($) b (stepper undefined e) <@ e)