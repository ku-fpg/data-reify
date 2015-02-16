{-# LANGUAGE CPP, TypeFamilies #-}
module Main (main) where

import           Control.Applicative hiding (Const)

import qualified Data.Foldable as F
#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid
#endif
import           Data.Reify
import qualified Data.Traversable as T

newtype Mu a = In (a (Mu a))

instance (T.Traversable a) => MuRef (Mu a) where
  type DeRef (Mu a) = a
--  deRef (In a) = a
  
  mapDeRef f (In a) = T.traverse f a

data List a b = Cons a b | Nil
        deriving Show
        
type MyList a = Mu (List a)

instance Functor (List a) where
   fmap _ Nil = Nil
   fmap f (Cons a b) = Cons a (f b)

instance F.Foldable (List a) where
   foldMap _ Nil        = mempty
   foldMap f (Cons _ b) = f b

instance T.Traversable (List a) where
  traverse f (Cons a b) = Cons <$> pure a <*> f b
  traverse _ Nil        = pure Nil

main :: IO ()
main = do
        let g1 :: MyList Int
            g1 = In (Cons 1 (In (Cons 2 (In Nil))))
        reifyGraph g1 >>= print
        let g2 :: MyList Int
            g2 =  In (Cons 1 (In (Cons 2 g2)))
        reifyGraph g2  >>= print
        let count n m | n == m    = In Nil
                      | otherwise = In (Cons n (count (succ n) m)) 
        let g3 :: MyList Int
            g3 = count 1 1000 
        reifyGraph g3  >>= print
        
        

