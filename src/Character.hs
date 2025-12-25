module Character where

import Point
import Face
import Polyhedron
import AlgebraicNum.AlgReal


volumeOnTriangle :: Point -> Point -> [Point] -> AlgReal
volumeOnTriangle _ _ [] = 0
volumeOnTriangle p1 p2 (p:ps) = abs (det p1 p2 p) / 6 + volumeOnTriangle p2 p ps

getVolume :: [Face] -> AlgReal
getVolume [] = 0
getVolume (f:fs) = volumeOnTriangle p1 p2 ps + getVolume fs
    where
        (p1:p2:ps) = points f
