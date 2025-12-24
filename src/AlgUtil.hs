module AlgUtil where

import Data.Ratio
import qualified Data.Vector as V (reverse, imap, fromList, toList)
import AlgebraicNum.AlgReal
import AlgebraicNum.UniPoly

-- toRealRoots :: AlgReal -> [AlgReal]
-- toRealRoots (AlgReal x _ _ _) = realRoots x
--
-- toSaveLoop :: Int -> [AlgReal] -> AlgReal -> Int
-- toSaveLoop n (y:ys) x
--     | y == x = n
--     | otherwise = toSaveLoop (n + 1) ys x
--
-- toSave :: AlgReal -> Int
-- toSave x = toSaveLoop 0 (toRealRoots x) x

toSave :: AlgReal -> [Int]
toSave (AlgReal (UniPoly p) n _ _) = n : map (\x -> fromIntegral x) (V.toList p)
toSave (FromRat r) = [0, fromIntegral (-1 * numerator r), fromInteger (denominator r)]

toValueLoop :: AlgReal -> Rational -> Rational -> Double
toValueLoop r a b = if a - b < 0.00000000000000001 then fromRational a :: Double else if r > m then toValueLoop r a mr else toValueLoop r mr b
    where
        m = FromRat ((a + b) / 2)
        mr = (a + b) / 2

toValue :: AlgReal -> Double
toValue (FromRat r) = fromRational r :: Double
toValue r = toValueLoop r a b
    where
        (AlgReal _ _ a b) = r

toLoadLoop :: Int -> [AlgReal] -> AlgReal
toLoadLoop n (y:ys)
    | n == 0 = y
    | otherwise = toLoadLoop (n - 1) ys

toLoad :: [Integer] -> Int -> AlgReal
toLoad xs n = toLoadLoop n (realRoots (UniPoly $ V.fromList xs))

toLoads :: [Integer] -> [AlgReal]
toLoads xs = realRoots (UniPoly $ V.fromList xs)
