module Character where

import Point
import Face
import Polyhedron
import AlgebraicNum.AlgReal


volumeOnTriangle :: Point -> Point -> [Point] -> AlgReal
volumeOnTriangle _ _ [] = 0
volumeOnTriangle p1 p2 (p:ps) = abs (det p1 p2 p) / 6 + volumeOnTriangle p2 p ps

volume :: [Face] -> AlgReal
volume [] = 0
volume (f:fs) = volumeOnTriangle p1 p2 ps + volume fs
    where
        (p1:p2:ps) = points f
