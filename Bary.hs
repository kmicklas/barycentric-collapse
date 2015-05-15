{-# LANGUAGE TupleSections #-}

module Bary where

import Data.List (sortBy, intercalate)
import Data.Set hiding (map, filter, foldr)
import qualified Data.Set as S
import Control.Monad

-- In order to get easier to read output than show:
class PrettyPrint a where
  pp :: a -> String

instance PrettyPrint Int where
  pp = show

instance PrettyPrint a => PrettyPrint (Set a) where
  pp s = "[" ++ intercalate "," (map pp $ toList s) ++ "]"

instance (PrettyPrint a, PrettyPrint b) => PrettyPrint (a, b) where
  pp (a, b) = pp a ++ ":" ++ pp b

-- Simplices and simplicial complexes:
type S v = Set v
type C v = Set (S v)

unionMap :: (Ord a, Ord b) => (a -> Set b) -> Set a -> Set b
unionMap f s = S.foldr (\ a s' -> union s' $ f a) empty s

subsetsOf :: (Ord a) => Set a -> Set (Set a)
subsetsOf s = S.foldr (\ v ss -> ss `union` S.map (insert v) ss) (singleton empty) s

-- Add smaller simplices to make a simplicial complex
close :: (PrettyPrint v, Ord v) => Set (S v) -> C v
close = unions . map subsetsOf . toList

simplex :: Int -> C Int
simplex n = close $ fromList $ map fromList [[0 .. n]]

sortBySize :: [Set a] -> [Set a]
sortBySize = sortBy (\ x y -> compare (size x) (size y))

bary :: (PrettyPrint v, Ord v) => C v -> C (S v)
bary = S.filter (orderedByInclusion . sortBySize . toList) . subsetsOf

orderedByInclusion :: (PrettyPrint v, Ord v) => [S v] -> Bool
orderedByInclusion [] = True
orderedByInclusion (x : xs) =
  orderedByInclusion xs && size x /= 0 && all (\ y -> x `isSubsetOf` y) xs

facet :: (PrettyPrint v, Ord v) => S v -> C v -> Bool
facet s c | member s c = size (S.filter (isSubsetOf s) c) == 1
facet s c | otherwise = error $ pp s ++ " not a simplex of " ++ pp c

facets :: (PrettyPrint v, Ord v) => C v -> Set (S v)
facets c | c == close c = S.filter ((flip facet) c) c
facets c | otherwise = error $ pp c ++ " not a simplicial complex"

freeFace :: (PrettyPrint v, Ord v) => S v -> C v -> Bool
freeFace s c | member s c && not (facet s c) =
  size (S.filter (\ f -> facet f c && s `isSubsetOf` f) c) == 1
freeFace s c | not $ member s c = error $ pp s ++ " not a simplex of " ++ pp (facets c)
freeFace s c | otherwise = error $ pp s ++ " is a facet not a free face of " ++ pp (facets c)

-- Perform an elementary collapse  of c with s against vertex v:
collapse :: (PrettyPrint v, Ord v) => C v -> S v -> C v
collapse c s | freeFace s c = S.filter (not . isSubsetOf s) c
collapse c s | otherwise = error $
  pp s ++ " is not a free face of " ++ pp (facets c) ++
  " because it is face of facets " ++ pp (S.filter (\ f -> facet f c && s `isSubsetOf` f) c)

-- Perform parallel elementary collapses:
collapseAll :: (PrettyPrint v, Ord v) => C v -> Set (S v) -> C v
collapseAll c ss = S.foldr (flip collapse) c ss

-- Subdivide and then collapse subdivided face:
baryCollapse :: (PrettyPrint v, Ord v) => C v -> S v -> v -> C (S v)
baryCollapse c s v = foldr (\ (d, n) c' -> collapseFaceDN d n c' s v)
                           (bary c)
                           (fromList $ do d <- [1 .. length s]
                                          map (d,) $ [0 .. d])

-- Subdivide and then collapse subdivided face:
baryCollapseStep :: (PrettyPrint v, Ord v) => C v -> S v -> v -> Int -> C (S v)
baryCollapseStep c s v n = foldr (\ (d, n) c' -> collapseFaceDN d n c' s v)
                                 (bary c)
                                 (fromList $ drop n $ do d <- [1 .. length s]
                                                         map (d,) $ [0 .. d])

-- Used to generate Mathematica for the diagrams in the paper
showSimplexCollapse n =
  do i <- reverse [0 .. ((n * (n + 1)) `quot` 2) + n]
     "c" ++ show i ++ " = Graphics3D[{Opacity[.5], " ++
       showCoords (baryCollapseStep (simplex n) (fromList [1 .. n]) 0 i) ++ "}, {Boxed -> False}]\n"

collapseFaceDN :: (PrettyPrint v, Ord v) => Int -> Int -> C (S v) -> S v -> v -> C (S v)
collapseFaceDN d n c s v = collapseAll c $ S.filter (faceDN d n) c
    where faceDN d n f =
            length f == d &&
            any (\ p -> s `isSubsetOf` p) f &&
            size (S.filter (\ p -> not $ member v p) f) == n &&
            all (differByMoreThanV f) f
          differByMoreThanV f p1 =
            all (\ p2 -> not $ member (difference p1 p2) $ fromList [singleton v, s]) f

baryCollapseValid :: (PrettyPrint v, Ord v) => C v -> S v -> v -> Bool
baryCollapseValid c s v = bary (collapse c s) == baryCollapse c s v

barySimplexCollapseValid :: Int -> Bool
barySimplexCollapseValid n = baryCollapseValid (simplex n) (fromList [1 .. n]) 0


coordPoint :: Int -> (Float, Float, Float)
coordPoint 0 = (0.0, 0.0, 0.0)
coordPoint 1 = (6.0, 0.0, 0.0)
coordPoint 2 = (0.0, 6.0, 0.0)
coordPoint 3 = (0.0, 0.0, 6.0)

coordS :: S Int -> (Float, Float, Float)
coordS s = (x / ss, y / ss, z / ss)
    where (x, y, z) = S.foldr (\ (a, b, c) (d, e, f) -> (a + d, b + e, c + f))
                              (0.0, 0.0, 0.0) $ S.map coordPoint s
          ss = fromIntegral $ size s

coordC :: C (S Int) -> Set (Set (Float, Float, Float))
coordC = S.map (S.map coordS)

showCoords c = ("{" ++) $ (++ "}") $ intercalate "," $ map showSimplex $ filter ((> 0) . size) $ toList $ coordC $ facets $ c
    where showSimplex s = ("Simplex[{" ++) $ (++ "}]") $ intercalate "," $ map showVertex $ toList s
          showVertex (x, y, z) = "{" ++ intercalate "," (map show [x, y, z]) ++ "}"

main = forM_ [1 .. 3] $ \ s -> print $ "Collapse of barycentric subdivision of " ++ show s ++
                                       " simplex valid: " ++ show (barySimplexCollapseValid s)
