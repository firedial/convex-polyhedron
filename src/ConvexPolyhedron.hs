module ConvexPolyhedron where

import Triangle
import AlgUtil
import Point
import Face
import Polyhedron


transformFaces :: [Point] -> [[Int]] -> [Face]
transformFaces points vertexes = map (transformFace points) vertexes
    where
        transformFace points vertex = Face $ map (\v -> points !! v) vertex

r1 :: Polyhedron
r1 = Polyhedron "r1" points (transformFaces points vertexes) vertexes
    where
        r = toLoad [-9, 0, 8] 1
        h = toLoad [-2, 0, 1] 1
        points = [Point cos0 sin0 (r - h), Point cos120 sin120 (r - h), Point cos240 sin240 (r - h), Point 0 0 r]
        vertexes = [[0, 1, 2], [0, 1, 3], [0, 2, 3], [1, 2, 3]]
