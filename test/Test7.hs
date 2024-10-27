{-# LANGUAGE TypeFamilies, UndecidableInstances,
             RankNTypes, ExistentialQuantification #-}
module Main (main) where

import Control.Applicative hiding (Const)

import Data.Reify

import System.CPUTime
import System.Environment

import Prelude

data Tree = Node Tree Tree | Leaf Int
         deriving (Show,Eq)

data T s = N s s | L Int

instance MuRef Tree where
  type DeRef Tree = T
  mapDeRef f (Node t1 t2) = N <$> f t1 <*> f t2
  mapDeRef _ (Leaf i)     = pure $ L i

deepTree :: Int -> Int -> Tree
deepTree 1 x = Leaf x
deepTree n x = Node (deepTree (pred n) (x * 37)) (deepTree (pred n) (x * 17))

-- no sharing
deepTree' :: Int -> Tree
deepTree' n = deepTree n 1

deepTree2 :: Int -> Integer -> Tree -> Tree
deepTree2 1 v x = if v == 89235872347 then Leaf 1 else x
deepTree2 n v x = Node (deepTree2 (pred n) (v * 37) x) (deepTree2 (pred n) (v * 17) x)

-- sharing
deepTree2' :: Int -> Tree
deepTree2' n = let v = deepTree2 n 1 v in v

timeme :: Int -> (Int -> Tree) -> IO Float
timeme n f = do
        i <- getCPUTime
        let g3 :: Tree
            g3 = f n
        reifyGraph g3 >>= \ (Graph xs _) -> putStr $ show (length xs)
        j <- getCPUTime
        let t :: Float
            t = fromIntegral ((j - i) `div` 1000000000)
        putStrLn $ " " ++ show n ++ " ==> " ++ show (t / 1000)
        return t

main :: IO ()
main = do
  (x:args) <- getArgs
  sequence_ [ timeme n (case x of
                         "sharing"    -> deepTree2'
                         "no-sharing" -> deepTree')
            | n <- map read args
            ]
