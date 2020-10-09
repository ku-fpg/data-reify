{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Reify (
        MuRef(..),
        module Data.Reify.Graph,
        reifyGraph,
        reifyGraphs
        ) where

import Control.Concurrent.MVar

import qualified Data.HashMap.Lazy as HM
import Data.HashMap.Lazy (HashMap)
import Data.Hashable as H
import Data.Reify.Graph
import qualified Data.IntSet as IS
import Data.IntSet (IntSet)

import System.Mem.StableName

#if !(MIN_VERSION_base(4,7,0))
import Unsafe.Coerce
#endif

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
import Data.Traversable
#endif

-- | 'MuRef' is a class that provided a way to reference into a specific type,
-- and a way to map over the deferenced internals.
class MuRef a where
  type DeRef a :: * -> *

  mapDeRef :: (Applicative f) =>
              (forall b . (MuRef b, DeRef a ~ DeRef b) => b -> f u)
                        -> a
                        -> f (DeRef a u)

-- | 'reifyGraph' takes a data structure that admits 'MuRef', and returns a 'Graph' that contains
-- the dereferenced nodes, with their children as 'Unique's rather than recursive values.
reifyGraph :: (MuRef s) => s -> IO (Graph (DeRef s))
reifyGraph m = do rt1 <- newMVar HM.empty
                  uVar <- newMVar 0
                  reifyWithContext rt1 uVar m

-- | 'reifyGraphs' takes a 'Traversable' container 't s' of a data structure 's'
-- admitting 'MuRef', and returns a 't (Graph (DeRef s))' with the graph nodes
-- resolved within the same context.
--
-- This allows for, e.g., a list of mutually recursive structures.
reifyGraphs :: (MuRef s, Traversable t) => t s -> IO (t (Graph (DeRef s)))
reifyGraphs coll = do rt1 <- newMVar HM.empty
                      uVar <- newMVar 0
                      traverse (reifyWithContext rt1 uVar) coll
                        -- NB: We deliberately reuse the same map of stable
                        -- names and unique supply across all iterations of the
                        -- traversal to ensure that the same context is used
                        -- when reifying all elements of the container.

-- Reify a data structure's 'Graph' using the supplied map of stable names and
-- unique supply.
reifyWithContext :: (MuRef s)
                 => MVar (HashMap DynStableName Unique)
                 -> MVar Unique
                 -> s
                 -> IO (Graph (DeRef s))
reifyWithContext rt1 uVar j = do
  rt2 <- newMVar []
  nodeSetVar <- newMVar IS.empty
  root <- findNodes rt1 rt2 uVar nodeSetVar j
  pairs <- readMVar rt2
  return (Graph pairs root)

-- The workhorse for 'reifyGraph' and 'reifyGraphs'.
findNodes :: (MuRef s)
          => MVar (HashMap DynStableName Unique)
             -- ^ A map of stable names to unique numbers.
             --   Invariant: all 'Uniques' that appear in the range are less
             --   than the current value in the unique name supply.
          -> MVar [(Unique,DeRef s Unique)]
             -- ^ The key-value pairs in the 'Graph' that is being built.
             --   Invariant 1: the domain of this association list is a subset
             --   of the range of the map of stable names.
             --   Invariant 2: the domain of this association list will never
             --   contain duplicate keys.
          -> MVar Unique
             -- ^ A supply of unique names.
          -> MVar IntSet
             -- ^ The unique numbers that we have encountered so far.
             --   Invariant: this set is a subset of the range of the map of
             --   stable names.
          -> s
             -- ^ The value for which we will reify a 'Graph'.
          -> IO Unique
             -- ^ The unique number for the value above.
findNodes rt1 rt2 uVar nodeSetVar !j = do
        st <- makeDynStableName j
        tab <- takeMVar rt1
        nodeSet <- takeMVar nodeSetVar
        case HM.lookup st tab of
          Just var -> do putMVar rt1 tab
                         if var `IS.member` nodeSet
                           then do putMVar nodeSetVar nodeSet
                                   return var
                           else recurse var nodeSet
          Nothing -> do var <- newUnique uVar
                        putMVar rt1 $ HM.insert st var tab
                        recurse var nodeSet
  where
    recurse :: Unique -> IntSet -> IO Unique
    recurse var nodeSet = do
      putMVar nodeSetVar $ IS.insert var nodeSet
      res <- mapDeRef (findNodes rt1 rt2 uVar nodeSetVar) j
      tab' <- takeMVar rt2
      putMVar rt2 $ (var,res) : tab'
      return var

newUnique :: MVar Unique -> IO Unique
newUnique var = do
  v <- takeMVar var
  let v' = succ v
  putMVar var v'
  return v'

-- Stable names that do not use phantom types.
-- As suggested by Ganesh Sittampalam.
-- Note: GHC can't unpack these because of the existential
-- quantification, but there doesn't seem to be much
-- potential to unpack them anyway.
data DynStableName = forall a. DynStableName !(StableName a)

instance Hashable DynStableName where
  hashWithSalt s (DynStableName n) = hashWithSalt s n

instance Eq DynStableName where
  DynStableName m == DynStableName n =
#if MIN_VERSION_base(4,7,0)
    eqStableName m n
#else
    m == unsafeCoerce n
#endif

makeDynStableName :: a -> IO DynStableName
makeDynStableName a = do
    st <- makeStableName a
    return $ DynStableName st
