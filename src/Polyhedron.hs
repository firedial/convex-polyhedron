module Polyhedron where

import AlgebraicNum.AlgReal
import Point
import Face

data Polyhedron = Polyhedron {ppoints :: [Point], pfaces :: [Face]}

isSamePlainList :: [Point] -> Bool
isSamePlainList (p1:p2:p3:p4:_) = isSamePlain p1 p2 p3 p4

-- isConvexPolyhedron :: Polyhedron -> Bool
-- isConvexPolyhedron ph
--     = length ps >= 4
--     && (and (filter (\p -> isSamePlainList p) (comb 4 ps)))
--     where
--         ps = ppoints ph
--         fs = pfaces ph

comb :: Int -> [a] -> [[a]]
comb 0 xs = [[]]
comb _ [] = []
comb n (x:xs) = [x:y | y <- comb (n-1) xs] ++ comb n xs
