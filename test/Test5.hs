{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main (main) where

import Control.Applicative hiding (Const)

import Data.Dynamic
import Data.Reify

import System.CPUTime

import Prelude

data List a b = Nil | Cons a b
  deriving Show

instance Typeable a => MuRef [a] where
  type DeRef [a] = List a 

  mapDeRef f (x:xs) = Cons x <$> f xs
  mapDeRef _ []     = pure Nil


instance Functor (List a) where
   fmap _ Nil = Nil
   fmap f (Cons a b) = Cons a (f b)

main :: IO ()
main = do
        let g1 = [1..(10::Int)]
        reifyGraph g1 >>= print
        let g2 = [1..(10::Int)] ++ g2
        reifyGraph g2 >>= print

        -- now, some timings.
        ns <- sequence [ timeme n | n <- take 8 (iterate (*2) 1024) ]
        print $ reverse $ take 4 $ reverse [ n2 / n1 | (n1,n2) <- zip ns (tail ns) ]

timeme :: Int -> IO Float
timeme n = do
        i <- getCPUTime
        let g3 = [1..n] ++ g3
        reifyGraph g3 >>= \ (Graph xs _) -> putStr $ show (length xs)
        j <- getCPUTime
        let n' :: Float
            n' = fromIntegral ((j - i) `div` 1000000000)
        putStrLn $ " ==> " ++ show (n' / 1000)   
        return n'
