module App.Utils (groupBy, padLeft, seriesSeqToLabel) where

import Data.Array (snoc)
import Data.Boolean (otherwise)
import Data.Bounded (class Ord)
import Data.EuclideanRing (div, mod, (-))
import Data.Foldable (class Foldable, foldl)
import Data.Function (($))
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Ord ((>=))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.String as S

groupBy
  :: ∀ f a b
   . Foldable f
  => Ord b
  => (a -> b)
  -> f a
  -> Map b (Array a)
groupBy eq f =
  foldl addToGroup Map.empty f
  where
    addToGroup m a =
      Map.alter (appendOrNew a) (eq a) m
    appendOrNew a Nothing = Just [a]
    appendOrNew a (Just as) = Just $ snoc as a

padLeft :: Int -> String -> String -> String
padLeft 0 c s = s
padLeft n c s = go n s
  where
    go 0 ps = ps
    go m ps
      | S.length ps >= n = ps
      | otherwise = go (m - 1) (c <> ps)

seriesSeqToLabel :: Int -> String
seriesSeqToLabel 0 = "A"
seriesSeqToLabel 1 = "B"
seriesSeqToLabel 2 = "C"
seriesSeqToLabel 3 = "D"
seriesSeqToLabel 4 = "E"
seriesSeqToLabel 5 = "F"
seriesSeqToLabel 6 = "G"
seriesSeqToLabel 7 = "H"
seriesSeqToLabel i =
  let rest = i `mod` 8
  in seriesSeqToLabel rest <> show (i `div` 8)
