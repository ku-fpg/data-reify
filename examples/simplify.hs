{-# LANGUAGE DeriveFunctor, DeriveFoldable, TypeFamilies,
      StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}
{-
 This example simplifies a reified graph so only nodes
 referenced from multiple places are assigned labels,
 and unshared terms are folded into the parent by
 changing the type of the graph to use the free
 monad (Free e) over the original functor e.
 -}
-- to define simplification
import Data.Foldable(Foldable,foldMap)
import Data.Monoid(Monoid(mempty,mappend,mconcat),(<>))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set

import Data.Reify(Graph(Graph),Unique)

-- for the example
import Control.Applicative(pure,liftA2)
import Data.Reify(MuRef(mapDeRef),DeRef,reifyGraph)

-- Self-contained Free monad
data Free f a = Pure a | Free (f (Free f a))
deriving instance (Show a, Show (f (Free f a))) => Show (Free f a)
instance (Functor f) => Monad (Free f) where
  return = Pure
  Pure a >>= f = f a
  Free m >>= f = Free (fmap (>>= f) m)

newtype Hist a = Hist (Map a Int)
  deriving Show
count :: a -> Hist a
count x = Hist (Map.singleton x 1)

instance (Ord a) => Monoid (Hist a) where
  mempty = Hist Map.empty
  mappend (Hist m1) (Hist m2) = Hist (Map.unionWith (+) m1 m2)
  mconcat hists = Hist (Map.unionsWith (+) [m | Hist m <- hists])

-- Count the number of times each Unique is referenced
-- in the graph.
occs :: (Foldable e) => Graph e -> Hist Unique
occs (Graph binds root) = count root <> foldMap (foldMap count . snd) binds

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
  mapDeRef child (Leaf v) = pure $ LeafF v
  mapDeRef child (Fork l r) = liftA2 ForkF (child l) (child r)

-- An example graph.
loop1, loop2 :: Tree Int

-- loop1 is referenced twice so it must have an explicit
-- label in the simplified graph whether or not it's the root.
loop1 = Fork (Fork (Leaf 1) loop1) loop2

-- loop2 is only reference once in the graph, so it will
-- have a label in the simplified graph only if it is the root.
loop2 = Fork loop1 (Leaf 2)

main = do
  putStrLn "Simplifed graph for loop1, should have one label"
  print . simpl =<< reifyGraph loop1
  putStrLn "Simplifed graph for loop2, should have two labels"
  print . simpl =<< reifyGraph loop2
