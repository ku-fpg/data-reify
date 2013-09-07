{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Monoid
import Control.Applicative hiding (Const)
import Data.Unique
import Data.Reify
import Control.Monad

-- Notice how there is nothing Mu-ish about this datatype.
data State a b = State a [(b,State a b)]
        deriving Show

s0 = State 0 [(True,s1),(False,s2)]
s1 = State 1 [(True,s0),(False,s1)]
s2 = State 2 [(True,s1),(False,s0)]

data StateDeRef a b r = StateDeRef a [(b,r)]
        deriving Show

instance MuRef (State a b) where
   type DeRef (State a b) = StateDeRef  a b
   mapDeRef f (State a tr) = StateDeRef a <$>
            T.traverse (\ (b,s) -> ((,) b) <$> (f s)) tr

instance Functor (StateDeRef a b) where
   fmap f (StateDeRef a tr) = StateDeRef a [ (b,f s) | (b,s) <- tr ]


main = do
        reifyGraph s0 >>= print

        
{- Alt:

data State s i o = State s [(i,o,State s i o)]
        deriving Show
        
state :: s -> State s i o
state s = State s []

infixl 4 %%
(%%) :: State s i o -> (i,o,State s i o) -> State s i o
(State s ts) %% (i,o,st) = State s $ (ts ++ [(i,o,st)])

s0 = state () %% (True,True,s1) %% (False,False,s0)
s1 = state () %% (True,False,s0) %% (False,True,s1)

data MuState s i o r = MuState s [(i,o,r)]
        deriving Show

-}
     

