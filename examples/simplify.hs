{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-
 This example simplifies a reified graph so only nodes
 referenced from multiple places are assigned labels,
 and unshared terms are folded into the parent by
 changing the type of the graph to use the free
 monad (Free e) over the original functor e.
 -}
module Main (main) where

-- to define simplification
import qualified Data.Map.Strict as Map
import           Data.Map.Strict (Map)
import           Data.Reify (Graph(Graph), Unique)
import qualified Data.Set as Set

-- for the example
import           Data.Reify (MuRef(mapDeRef), DeRef, reifyGraph)

#if !(MIN_VERSION_base(4,8,0))
import           Control.Applicative (Applicative(..))
import           Data.Foldable (Foldable,foldMap)
import           Data.Functor ((<$>))
import           Data.Monoid (Monoid(..))
#endif

#if MIN_VERSION_base(4,9,0) && !(MIN_VERSION_base(4,11,0))
import           Data.Semigroup (Semigroup(..))
#endif

#if !(MIN_VERSION_base(4,18,0))
import           Control.Applicative (liftA2)
#endif

-- Self-contained Free monad
data Free f a = Pure a | Free (f (Free f a))
deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)

instance Functor f => Functor (Free f) where
  fmap f = go where
    go (Pure a)  = Pure (f a)
    go (Free fa) = Free (go <$> fa)

instance Functor f => Applicative (Free f) where
  pure = Pure
  Pure a <*> Pure b = Pure $ a b
  Pure a <*> Free mb = Free $ fmap a <$> mb
  Free ma <*> b = Free $ (<*> b) <$> ma

instance Functor f => Monad (Free f) where
#if !(MIN_VERSION_base(4,11,0))
  return = Pure
#endif
  Pure a >>= f = f a
  Free m >>= f = Free (fmap (>>= f) m)

newtype Hist a = Hist (Map a Int)
  deriving Show
count :: a -> Hist a
count x = Hist (Map.singleton x 1)

#if __GLASGOW_HASKELL__ >= 800
instance (Ord a) => Semigroup (Hist a) where
  (<>) (Hist m1) (Hist m2) = Hist (Map.unionWith (+) m1 m2)
#endif

instance (Ord a) => Monoid (Hist a) where
  mempty = Hist Map.empty
#if !(MIN_VERSION_base(4,11,0))
  mappend (Hist m1) (Hist m2) = Hist (Map.unionWith (+) m1 m2)
#endif
  mconcat hists = Hist (Map.unionsWith (+) [m | Hist m <- hists])

-- Count the number of times each Unique is referenced
-- in the graph.
occs :: (Foldable e) => Graph e -> Hist Unique
occs (Graph binds root) = count root `mappend` foldMap (foldMap count . snd) binds

-- nest unshared nodes into parents.
simpl :: (Functor e, Foldable e) => Graph e -> Graph (Free e)
simpl g@(Graph binds root) =
  let Hist counts = occs g
      repeated = Map.keysSet (Map.filter (>1) counts)
      grow ix
        | Set.member ix repeated = Pure ix
        | otherwise =
            case lookup ix binds of
              Just pat -> Free (fmap grow pat)
              Nothing -> error "this shouldn't happen"
  in Graph [(k, Free (fmap grow v))
           | (k,v) <- binds, Set.member k repeated]
     root

-- A data type for the example.
data Tree a =
    Leaf a
  | Fork (Tree a) (Tree a)
  deriving (Show)
data TreeF a t =
    LeafF a
  | ForkF t t
  deriving (Show, Functor, Foldable)
instance MuRef (Tree a) where
  type DeRef (Tree a) = TreeF a
  mapDeRef _     (Leaf v) = pure $ LeafF v
  mapDeRef child (Fork l r) = liftA2 ForkF (child l) (child r)

-- An example graph.
loop1, loop2 :: Tree Int

-- loop1 is referenced twice so it must have an explicit
-- label in the simplified graph whether or not it's the root.
loop1 = Fork (Fork (Leaf 1) loop1) loop2

-- loop2 is only reference once in the graph, so it will
-- have a label in the simplified graph only if it is the root.
loop2 = Fork loop1 (Leaf 2)

main :: IO ()
main = do
  putStrLn "Simplifed graph for loop1, should have one label"
  print . simpl =<< reifyGraph loop1
  putStrLn "Simplifed graph for loop2, should have two labels"
  print . simpl =<< reifyGraph loop2
