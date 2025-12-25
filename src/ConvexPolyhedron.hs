module ConvexPolyhedron where

import Triangle
import AlgUtil
import Point
import Face
import Polyhedron

getPolyhedrons :: [Polyhedron]
getPolyhedrons = [r1, r2]

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

r2 :: Polyhedron
r2 = Polyhedron "r2" points (transformFaces points vertexes) vertexes
    where
        h = toLoad [-1, 0, 2] 1
        points = [
            Point cos0 sin0 h,
            Point cos90 sin90 h,
            Point cos180 sin180 h,
            Point cos270 sin270 h,
            Point cos0 sin0 (-1 * h),
            Point cos90 sin90 (-1 * h),
            Point cos180 sin180 (-1 * h),
            Point cos270 sin270 (-1 * h)
            ]
        vertexes = [[0, 1, 2, 3], [0, 1, 5, 4], [1, 2, 6, 5], [2, 3, 7, 6], [3, 0, 4, 7], [4, 5, 6, 7]]
