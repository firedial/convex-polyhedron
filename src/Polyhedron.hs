module Polyhedron where

import AlgebraicNum.AlgReal
import Point
import Face

data Polyhedron = Polyhedron {ppoints :: [Point], pfaces :: [Face]}

isSamePlainList :: [Point] -> Bool
isSamePlainList (p1:p2:p3:p4:_) = isSamePlain p1 p2 p3 p4

hasPoint :: [Face] -> [Point] -> Bool
hasPoint faces [] = True
hasPoint faces (p:ps) = if isContained then hasPoint faces ps else False
    where
        isContained = or (map (\f -> elem p (points f)) faces)

getEdgesFromFace :: Point -> [Point] -> [(Point, Point)]
getEdgesFromFace pp [] = []
getEdgesFromFace pp (p:ps) = (pp, p) : getEdgesFromFace p ps

getEdges :: [Face] -> [(Point, Point)]
getEdges [] = []
getEdges (f:fs) = (getEdgesFromFace (last $ points f) (points f)) ++ (getEdges fs)

isTwoEdge :: [(Point, Point)] -> [(Point, Point)] -> [(Point, Point)] -> Bool
isTwoEdge f s [] = length f == length s
isTwoEdge f s (p:ps)
    | (not (elem p f)) && (not (elem (snd p, fst p) f)) = isTwoEdge (p : f) s ps
    | (not (elem p s)) && (not (elem (snd p, fst p) s)) = isTwoEdge f (p : s) ps
    | otherwise = False

isConvexPolyhedron :: Polyhedron -> Bool
isConvexPolyhedron ph
    = length ps >= 4
    && and (map (\f -> isRegularFace f) fs)
    && hasPoint fs ps
    && isTwoEdge [] [] (getEdges fs)
    -- && (and (map (\p -> isSamePlainList p) (comb 4 ps)))
    where
        ps = ppoints ph
        fs = pfaces ph

comb :: Int -> [a] -> [[a]]
comb 0 xs = [[]]
comb _ [] = []
comb n (x:xs) = [x:y | y <- comb (n-1) xs] ++ comb n xs
