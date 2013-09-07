-- |
-- Module: Data.Reify.Graph
-- Copyright: (c) 2009 Andy Gill
-- License: BSD3
--
-- Maintainer: Andy Gill <andygill@ku.edu>
-- Stability: unstable
-- Portability: ghc
--
-- This is the shared definition of a 'Graph' in Data.Reify.


{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}

module Data.Reify.Graph (
        Graph(..),
        Unique
        ) where

-- | 'Graph' is a basic graph structure over nodes of the higher kind 'e', with a single root.
-- There is an assumption that there is no Unique used in a node which does not have a 
-- corresponding entry is the association list.
-- The idea with this structure is that it is trivial to convert into an 'Array', 
-- 'IntMap', or into a Martin Erwig's Functional Graph, as required.   

data Graph e = Graph [(Unique,e Unique)] Unique


type Unique = Int

-- | If 'e' is s Functor, and 'e' is 'Show'-able, then we can 'Show' a 'Graph'.
instance (Show (e Int)) => Show (Graph e) where
  show (Graph netlist start) = "let " ++ show [ (u,e)
                                              | (u,e) <- netlist 
                                              ] ++ " in " ++ show start

