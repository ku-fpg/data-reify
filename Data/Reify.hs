{-# LANGUAGE CPP, TypeFamilies, RankNTypes #-}
module Data.Reify (
        MuRef(..),
        module Data.Reify.Graph,
        reifyGraph,
        reifyGraphs
        ) where

import Control.Applicative
import Control.Concurrent.MVar

import Data.IntMap as M
import Data.Reify.Graph
import qualified Data.Set as S
#if !(MIN_VERSION_base(4,8,0))
import Data.Traversable
#endif

import System.Mem.StableName

import Unsafe.Coerce

import Prelude

-- | 'MuRef' is a class that provided a way to reference into a specific type,
-- and a way to map over the deferenced internals.

class MuRef a where
  type DeRef a :: * -> *

  mapDeRef :: (Applicative f) =>
              (forall b . (MuRef b, DeRef a ~ DeRef b) => b -> f u)
                        -> a
                        -> f (DeRef a u)

-- | 'reifyGraph' takes a data structure that admits 'MuRef', and returns a 'Graph' that contains
-- the dereferenced nodes, with their children as 'Int' rather than recursive values.

reifyGraph :: (MuRef s) => s -> IO (Graph (DeRef s))
reifyGraph m = do rt1 <- newMVar M.empty
                  rt2 <- newMVar []
                  uVar <- newMVar 0
                  reifyWithContext rt1 rt2 uVar m

-- | 'reifyGraphs' takes a 'Traversable' container 't s' of a data structure 's'
-- admitting 'MuRef', and returns a 't (Graph (DeRef s))' with the graph nodes
-- resolved within the same context.
--
-- This allows for, e.g., a list of mutually recursive structures.
reifyGraphs :: (MuRef s, Traversable t) => t s -> IO (t (Graph (DeRef s)))
reifyGraphs coll = do rt1 <- newMVar M.empty
                      uVar <- newMVar 0
                      flip traverse coll $ \m -> do
                        rt2 <- newMVar []
                        reifyWithContext rt1 rt2 uVar m

reifyWithContext :: (MuRef s)
          => MVar (IntMap [(DynStableName,Int)])
          -> MVar [(Int,DeRef s Int)]
          -> MVar Int
          -> s
          -> IO (Graph (DeRef s))
reifyWithContext rt1 rt2 uVar j = do
  root <- findNodes rt1 rt2 uVar S.empty j
  pairs <- readMVar rt2
  return (Graph pairs root)


findNodes :: (MuRef s)
          => MVar (IntMap [(DynStableName,Int)])
          -> MVar [(Int,DeRef s Int)]
          -> MVar Int
          -> S.Set Int
          -> s
          -> IO Int
findNodes rt1 rt2 uVar nodeSet j | j `seq` True = do
        st <- makeDynStableName j
        tab <- takeMVar rt1
        case mylookup st tab of
          Just var -> do putMVar rt1 tab
                         if var `S.member` nodeSet
                           then return var
                           else do res <- mapDeRef (findNodes rt1 rt2 uVar (S.insert var nodeSet)) j
                                   tab' <- takeMVar rt2
                                   putMVar rt2 $ (var,res) : tab'
                                   return var
          Nothing ->
                    do var <- newUnique uVar
                       putMVar rt1 $ M.insertWith (++) (hashDynStableName st) [(st,var)] tab
                       res <- mapDeRef (findNodes rt1 rt2 uVar (S.insert var nodeSet)) j
                       tab' <- takeMVar rt2
                       putMVar rt2 $ (var,res) : tab'
                       return var
findNodes _ _ _ _ _ = error "findNodes: strictness seq function failed to return True"

mylookup :: DynStableName -> IntMap [(DynStableName,Int)] -> Maybe Int
mylookup h tab =
           case M.lookup (hashDynStableName h) tab of
             Just tab2 -> Prelude.lookup h [ (c,u) | (c,u) <- tab2 ]
             Nothing ->  Nothing

newUnique :: MVar Int -> IO Int
newUnique var = do
  v <- takeMVar var
  let v' = succ v
  putMVar var v'
  return v'

-- Stable names that not use phantom types.
-- As suggested by Ganesh Sittampalam.
data DynStableName = DynStableName (StableName ())

hashDynStableName :: DynStableName -> Int
hashDynStableName (DynStableName sn) = hashStableName sn

instance Eq DynStableName where
    (DynStableName sn1) == (DynStableName sn2) = sn1 == sn2

makeDynStableName :: a -> IO DynStableName
makeDynStableName a = do
    st <- makeStableName a
    return $ DynStableName (unsafeCoerce st)
