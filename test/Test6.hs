{-# LANGUAGE TypeFamilies, UndecidableInstances, DeriveDataTypeable, RankNTypes, ExistentialQuantification      #-}
module Main where

import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Monoid
--import Control.Monad
import Control.Applicative hiding (Const)

import Data.Reify
import Control.Monad
import System.CPUTime
import Data.Typeable
import Control.Exception as E


import Data.Dynamic

data List b = Nil | Cons b b | Int Int | Lambda b b | Var | Add b b
  deriving Show

instance MuRef Int where
  type DeRef Int = List 

  mapDeRef f n = pure $ Int n

instance (Typeable a, MuRef a,DeRef [a] ~ DeRef a) => MuRef [a] where
  type DeRef [a] = List 
  
  mapDeRef f (x:xs) = liftA2 Cons (f x) (f xs)
  mapDeRef f []     = pure Nil


instance NewVar Exp where
  mkVar = ExpVar
--          return $ Var $ toDyn fn

data Exp = ExpVar Dynamic | ExpLit Int | ExpAdd Exp Exp
  deriving (Typeable, Show)
  
  
instance Eq Exp where
    _ == _ = False
    
-- instance Eq Dynamic where { a == b = False }

instance MuRef Exp where
  type DeRef Exp = List
  
  mapDeRef f (ExpVar _)   = pure Var
  mapDeRef f (ExpLit i)   = pure $ Int i
  mapDeRef f (ExpAdd x y) = Add <$> f x <*> f y


instance Num Exp where
    (+) = ExpAdd
    fromInteger n = ExpLit (fromInteger n)
    
instance (MuRef a,Typeable a, NewVar a, Typeable b, MuRef b, DeRef a ~ DeRef (a -> b),DeRef b ~ DeRef (a -> b)) => MuRef (a -> b) where
  type DeRef (a -> b) = List

  mapDeRef f fn = let v = mkVar $ toDyn fn 
                  in Lambda <$> f v <*> f (fn v)

class NewVar a where
  mkVar :: Dynamic -> a

instance Functor (List) where
   fmap f Nil = Nil
   fmap f (Cons a b) = Cons (f a) (f b)
   fmap f (Int n)    = Int n
   fmap f (Lambda a b) = Lambda (f a) (f b)
   fmap f Var   = Var
   fmap f (Add a b) = Add (f a) (f b)

main = do
        let g1 :: [Int]
            g1 = [1..10]
        reifyGraph g1 >>= print
        let g2 :: [Int]
            g2 = [1..10] ++ g2
        reifyGraph g2 >>= print

        let g3 = [\ x -> x :: Exp, \ y -> y + head g3 2] ++ g3
        reifyGraph g3 >>= print
        
        -- now, some timings.
        ns <- sequence [ timeme n | n <- take 8 (iterate (*2) 1024) ]
        print $ reverse $ take 4 $ reverse [ n2 / n1 | (n1,n2) <- zip ns (tail ns) ]

zz = let xs = [1..3] 
         ys = (0::Int) : xs
     in cycle [xs,ys,tail ys]
timeme n = do
        i <- getCPUTime
        let g3 :: [Int]
            g3 = [1..n] ++ g3
        reifyGraph g3 >>= \ (Graph xs _) -> putStr $ show (length xs)
        j <- getCPUTime
        let n :: Float
            n = fromIntegral ((j - i) `div` 1000000000)
        putStrLn $ " ==> " ++ show (n / 1000)   
        return n    
        
capture :: (Typeable a, Typeable b, NewVar a) => (a -> b) -> (a,b)
capture f = (a,f a)
  where a = mkVar (toDyn f)          