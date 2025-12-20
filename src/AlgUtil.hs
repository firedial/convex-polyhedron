module AlgUtil where

import qualified Data.Vector as V (reverse, imap, fromList)
import AlgebraicNum.AlgReal
import AlgebraicNum.UniPoly

toRealRoots :: AlgReal -> [AlgReal]
toRealRoots (AlgReal x _ _ _) = realRoots x

toSaveLoop :: Int -> [AlgReal] -> AlgReal -> Int
toSaveLoop n (y:ys) x
    | y == x = n
    | otherwise = toSaveLoop (n + 1) ys x

toSave :: AlgReal -> Int
toSave x = toSaveLoop 0 (toRealRoots x) x

toLoadLoop :: Int -> [AlgReal] -> AlgReal
toLoadLoop n (y:ys)
    | n == 0 = y
    | otherwise = toLoadLoop (n - 1) ys

toLoad :: [Integer] -> Int -> AlgReal
toLoad xs n = toLoadLoop n (realRoots (UniPoly $ V.fromList xs))

toLoads :: [Integer] -> [AlgReal]
toLoads xs = realRoots (UniPoly $ V.fromList xs)
