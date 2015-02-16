{-# LANGUAGE CPP, TypeFamilies #-}
module Main (main) where

import           Control.Applicative hiding (Const)

import qualified Data.Foldable as F
#if !(MIN_VERSION_base(4,8,0))
import           Data.Monoid
#endif
import           Data.Reify
import qualified Data.Traversable as T

data Signal = Signal (Circuit Signal)

-- Call this 'Circuit'
data Circuit c
 = And2 (c,c)
 | Xor2 (c,c)
 | Mux2 c (c,c)
 | Delay c
 | Const BitValue
 | Var String
        deriving (Eq,Ord)

-- newtype Mu a = In (a (Mu a))

instance MuRef Signal where
  type DeRef Signal = Circuit
  
  mapDeRef f (Signal s) = T.traverse f s
 
instance Show Signal where
  show (Signal b) = show b

instance Show c => Show (Circuit c) where
  show (Const bv)       = show bv
  show (And2 (b1,b2))   = "and(" ++ show b1 ++ "," ++ show b2 ++ ")"
  show (Xor2 (b1,b2))   = "xor(" ++ show b1 ++ "," ++ show b2 ++ ")"
  show (Mux2 s (b1,b2)) = "mux(" ++ show s ++ "," ++ show b1 ++ "," ++ show b2 ++ ")"
  show (Delay b)        = "delay(" ++ show b ++ ")"
  show (Var str)        = show str
  
and2 :: (Signal, Signal) -> Signal
and2 (s1,s2) = Signal (And2 (s1,s2))

xor2 :: (Signal, Signal) -> Signal
xor2 (s1,s2) = Signal (Xor2 (s1,s2))

mux2 :: Signal -> (Signal, Signal) -> Signal
mux2 s (s1,s2) = Signal (Mux2 s (s1,s2))

-- delay :: Signal -> Signal
-- delay s = Signal (Delay s)

pad :: String -> Signal
pad nm = Signal (Var nm)

data BitValue = High | Low
        deriving (Eq,Ord)

high, low :: Signal
high = Signal $ Const High
low  = Signal $ Const Low

instance Show BitValue where
   show High = "high"
   show Low  = "low"

halfAdder :: (Signal,Signal) -> (Signal,Signal)
halfAdder (a,b) = (carry,sum')
  where carry = and2 (a,b)
        sum'  = xor2 (a,b)

fullAdder :: (Signal,(Signal,Signal)) -> (Signal,Signal)
fullAdder (cin,(a,b)) = (cout,sum')
  where (car1,sum1) = halfAdder (a,b)
        (car2,sum') = halfAdder (cin,sum1)
        cout        = xor2 (car1,car2)
           
instance F.Foldable Circuit where
   foldMap f (And2 (e1,e2))   = f e1 `mappend`  f e2
   foldMap f (Xor2 (e1,e2))   = f e1 `mappend`  f e2
   foldMap f (Mux2 s (e1,e2)) = f s `mappend` f e1 `mappend`  f e2
   foldMap f (Delay s)        = f s
   foldMap _ (Const _)        = mempty
   foldMap _ (Var _)          = mempty


instance Functor Circuit where
   fmap f (And2 (e1,e2))   = And2 (f e1,f e2)
   fmap f (Xor2 (e1,e2))   = Xor2 (f e1,f e2)
   fmap f (Mux2 s (e1,e2)) = Mux2 (f s) (f e1,f e2)
   fmap f (Delay s)        = Delay (f s)
   fmap _ (Const a)        = Const a
   fmap _ (Var a)          = Var a

instance T.Traversable Circuit where
  traverse f (And2 (e1,e2))   = (\ x y -> And2 (x,y)) <$> f e1 <*> f e2
  traverse f (Xor2 (e1,e2))   = (\ x y -> Xor2 (x,y))  <$> f e1 <*> f e2
  traverse f (Mux2 c (e1,e2)) = (\ c' x y -> Mux2 c' (x,y)) <$> f c <*> f e1 <*> f e2
  traverse f (Delay s)        = Delay <$> f s
  traverse _ (Const a)        = pure (Const a)
  traverse _ (Var a)          = pure (Var a)

rowLA :: (Signal -> (b,b) -> b) -> ((Signal,a) -> (Signal,b)) -> (Signal,[a]) ->
 (Signal,[b])
rowLA _     _ (cin,[])   = (cin,[])
rowLA _     f (cin,[a])  = (car,[sum'])
  where (car,sum')  = f (cin,a)
rowLA mymux f (cin,cs)   = (mux2 cout1 (cout2_lo,cout2_hi),
                    sums1 ++ 
                        [ mymux cout1 (s_lo,s_hi)
                        | (s_lo,s_hi) <- zip sums2_lo sums2_hi
                        ])
  where
    len = length cs `div` 2
    (cout1,sums1) = rowLA mymux f (cin,take len cs)
    (cout2_hi,sums2_hi) = rowLA mymux f (high,drop len cs)
    (cout2_lo,sums2_lo) = rowLA mymux f (low,drop len cs)

main :: IO ()
main = do
        let g1 = xor2 (xor2 (pad "a",pad "b"),g1)
        reifyGraph g1 >>= print
        let (g2,_) = rowLA mux2 fullAdder
                                (pad "c",[ (pad $ "a" ++ show x,pad $ "b" ++ show x)
                                     | x <- [1..20] :: [Int]
                                     ])
        reifyGraph g2  >>= print


