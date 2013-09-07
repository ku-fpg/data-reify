{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Monoid
import Control.Applicative hiding (Const)
import Data.Unique
import Data.Reify

newtype Mu a = In (a (Mu a))

instance (T.Traversable a) => MuRef (Mu a) where
  type DeRef (Mu a) = a
--  deRef (In a) = a
  
  mapDeRef f (In a) = T.traverse f a

data List a b = Cons a b | Nil
        deriving Show
        
type MyList a = Mu (List a)

instance Functor (List a) where
   fmap f Nil = Nil
   fmap f (Cons a b) = Cons a (f b)

instance F.Foldable (List a) where
   foldMap f Nil        = mempty
   foldMap f (Cons a b) = f b

instance T.Traversable (List a) where
  traverse f (Cons a b) = Cons <$> pure a <*> f b
  traverse f Nil        = pure Nil


main = do
        let g1 :: MyList Int
            g1 = In (Cons 1 (In (Cons 2 (In Nil))))
        reifyGraph g1 >>= print
        let g2 =  In (Cons 1 (In (Cons 2 g2)))
        reifyGraph g2  >>= print
        let count n m | n == m    = In Nil
                      | otherwise = In (Cons n (count (succ n) m)) 
        let g3 = count 1 1000 
        reifyGraph g3  >>= print
        
        

