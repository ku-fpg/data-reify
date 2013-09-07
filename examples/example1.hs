{-# LANGUAGE TypeFamilies, DeriveFunctor, DeriveFoldable, DeriveTraversable #-}
import Control.Applicative
import Data.Reify
import Data.Traversable
import System.FilePath
import Data.IntMap as IntMap
import Data.Foldable
import Data.Monoid

{-
This example was written by Edward Kmett for Johan Tibell,
and can be found at http://lpaste.net/74064

-}


data DistF a
  = ConcatF [a]
  | ConcatMapF String [a]
  | GroupByKeyF [a]
  | InputF FilePath
  deriving (Functor, Foldable, Traversable)

newtype Dist a = Dist (DistF (Dist a))

instance MuRef (Dist a) where
  type DeRef (Dist a) = DistF
  mapDeRef f (Dist body) = case body of
    ConcatF xs      -> ConcatF <$> traverse f xs
    ConcatMapF n xs -> ConcatMapF n <$> traverse f xs
    GroupByKeyF xs  -> GroupByKeyF <$> traverse f xs
    InputF fn       -> pure (InputF fn)

data D
  = Concat [D]
  | ConcatMap String [D]
  | GroupByKey [D]
  | Input FilePath
  | Var Int

share :: Dist a -> IO (IntMap D, D)
share d = do
  Graph nodes s <- reifyGraph d
  let universe = IntMap.fromList nodes
      refs = insertWith (+) s 1 $ Prelude.foldr (\k -> insertWith (+) (fst k) 1) mempty nodes
      (urefs, mrefs) = IntMap.partition (==1) refs
      lut = intersectionWith const universe urefs
  return (mapWithKey (\k _ -> expand lut k) mrefs, expand lut s)

expand :: IntMap (DistF Int) -> Int -> D
expand m = go where
  go k = case IntMap.lookup k m of
    Nothing -> Var k
    Just d -> case d of
      ConcatF xs      -> Concat (go <$> xs)
      ConcatMapF n xs -> ConcatMap n (go <$> xs)
      GroupByKeyF xs  -> GroupByKey (go <$> xs)
      InputF fn       -> Input fn