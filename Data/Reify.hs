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
                  rt2 <- newMVar []
                  uVar <- newMVar 0
                  reifyWithContext rt1 rt2 uVar m

-- | 'reifyGraphs' takes a 'Traversable' container 't s' of a data structure 's'
-- admitting 'MuRef', and returns a 't (Graph (DeRef s))' with the graph nodes
-- resolved within the same context.
--
-- This allows for, e.g., a list of mutually recursive structures.
reifyGraphs :: (MuRef s, Traversable t) => t s -> IO (t (Graph (DeRef s)))
reifyGraphs coll = do rt1 <- newMVar HM.empty
                      uVar <- newMVar 0
                      flip traverse coll $ \m -> do
                        rt2 <- newMVar []
                        reifyWithContext rt1 rt2 uVar m

reifyWithContext :: (MuRef s)
          => MVar (HashMap DynStableName Unique)
          -> MVar [(Unique,DeRef s Unique)]
          -> MVar Unique
          -> s
          -> IO (Graph (DeRef s))
reifyWithContext rt1 rt2 uVar j = do
  root <- findNodes rt1 rt2 uVar IS.empty j
  pairs <- readMVar rt2
  return (Graph pairs root)

findNodes :: (MuRef s)
          => MVar (HashMap DynStableName Unique)
          -> MVar [(Unique,DeRef s Unique)]
          -> MVar Unique
          -> IntSet
          -> s
          -> IO Unique
findNodes rt1 rt2 uVar nodeSet !j = do
        st <- makeDynStableName j
        tab <- takeMVar rt1
        case HM.lookup st tab of
          Just var -> do putMVar rt1 tab
                         if var `IS.member` nodeSet
                           then return var
                           else do res <- mapDeRef (findNodes rt1 rt2 uVar (IS.insert var nodeSet)) j
                                   tab' <- takeMVar rt2
                                   putMVar rt2 $ (var,res) : tab'
                                   return var
          Nothing ->
                    do var <- newUnique uVar
                       putMVar rt1 $ HM.insert st var tab
                       res <- mapDeRef (findNodes rt1 rt2 uVar (IS.insert var nodeSet)) j
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
