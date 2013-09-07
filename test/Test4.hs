{-# LANGUAGE TypeFamilies #-}
module Main where

import qualified Data.Traversable as T
import qualified Data.Foldable as F
import Data.Monoid
--import Control.Monad
import Control.Applicative hiding (Const)

import Data.Reify
import Control.Monad
import System.CPUTime

data List a b = Nil | Cons a b
  deriving Show


instance MuRef [a] where
  type DeRef [a] = List a 

  mapDeRef f (x:xs) = Cons x <$> f xs
  mapDeRef f []     = pure Nil
  
instance Functor (List a) where
   fmap f Nil = Nil
   fmap f (Cons a b) = Cons a (f b)

main = do
        let g1 = [1..10]
        reifyGraph g1 >>= print
        let g2 = [1..10] ++ g2
        reifyGraph g2 >>= print

        -- now, some timings.
        ns <- sequence [ timeme n | n <- take 8 (iterate (*2) 1024) ]
        print $ reverse $ take 4 $ reverse [ n2 / n1 | (n1,n2) <- zip ns (tail ns) ]

timeme n = do
        i <- getCPUTime
        let g3 = [1..n] ++ g3
        reifyGraph g3 >>= \ (Graph xs _) -> putStr $ show (length xs)
        j <- getCPUTime
        let n :: Float
            n = fromIntegral ((j - i) `div` 1000000000)
        putStrLn $ " ==> " ++ show (n / 1000)   
        return n    