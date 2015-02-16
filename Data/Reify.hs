{-# LANGUAGE  TypeFamilies, RankNTypes #-}
module Data.Reify (
        MuRef(..),
        module Data.Reify.Graph,
        reifyGraph
        ) where

import Control.Applicative
import Control.Concurrent.MVar

import Data.IntMap as M
import Data.Reify.Graph

import System.Mem.StableName

import Unsafe.Coerce


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
                  root <- findNodes rt1 rt2 uVar m
                  pairs <- readMVar rt2
                  return (Graph pairs root)

findNodes :: (MuRef s) 
          => MVar (IntMap [(DynStableName,Int)])  
          -> MVar [(Int,DeRef s Int)] 
          -> MVar Int
          -> s 
          -> IO Int
findNodes rt1 rt2 uVar j | j `seq` True = do
        st <- makeDynStableName j
        tab <- takeMVar rt1
        case mylookup st tab of
          Just var -> do putMVar rt1 tab
                         return $ var
          Nothing -> 
                    do var <- newUnique uVar
                       putMVar rt1 $ M.insertWith (++) (hashDynStableName st) [(st,var)] tab
                       res <- mapDeRef (findNodes rt1 rt2 uVar) j
                       tab' <- takeMVar rt2
                       putMVar rt2 $ (var,res) : tab'
                       return var
findNodes _ _ _ _ = error "findNodes: strictness seq function failed to return True"

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
