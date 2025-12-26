module ConvexPolyhedron where

import Triangle
import AlgUtil
import Point
import Face
import Polyhedron

getPolyhedrons :: [Polyhedron]
-- getPolyhedrons = [r1, r2]
getPolyhedrons = [p8]

transformFaces :: [Point] -> [[Int]] -> [Face]
transformFaces points vertexes = map (transformFace points) vertexes
    where
        transformFace points vertex = Face $ map (\v -> points !! v) vertex

p3 :: Polyhedron
p3 = Polyhedron "p3" points (transformFaces points vertexes) vertexes
    where
        h = toLoad [-3, 0, 4] 1
        points = [
            Point cos0 sin0 h,
            Point cos120 sin120 h,
            Point cos240 sin240 h,
            Point cos0 sin0 (-1 * h),
            Point cos120 sin120 (-1 * h),
            Point cos240 sin240 (-1 * h)
            ]
        vertexes = [[0, 1, 2], [0, 1, 4, 3], [1, 2, 5, 4], [2, 0, 3, 5], [3, 4, 5]]

p5 :: Polyhedron
p5 = Polyhedron "p5" points (transformFaces points vertexes) vertexes
    where
        h = sin36
        points = [
            Point cos0 sin0 h,
            Point cos72 sin72 h,
            Point cos144 sin144 h,
            Point cos216 sin216 h,
            Point cos288 sin288 h,
            Point cos0 sin0 (-1 * h),
            Point cos72 sin72 (-1 * h),
            Point cos144 sin144 (-1 * h),
            Point cos216 sin216 (-1 * h),
            Point cos288 sin288 (-1 * h)
            ]
        vertexes = [[0, 1, 2, 3 ,4], [0, 1, 6, 5], [1, 2, 7, 6], [2, 3, 8, 7], [3, 4, 9, 8], [4, 0, 5, 9], [5, 6, 7, 8, 9]]

p6 :: Polyhedron
p6 = Polyhedron "p6" points (transformFaces points vertexes) vertexes
    where
        h = sin30
        points = [
            Point cos0 sin0 h,
            Point cos60 sin60 h,
            Point cos120 sin120 h,
            Point cos180 sin180 h,
            Point cos240 sin240 h,
            Point cos300 sin300 h,
            Point cos0 sin0 (-1 * h),
            Point cos60 sin60 (-1 * h),
            Point cos120 sin120 (-1 * h),
            Point cos180 sin180 (-1 * h),
            Point cos240 sin240 (-1 * h),
            Point cos300 sin300 (-1 * h)
            ]
        vertexes = [[0, 1, 2, 3 ,4, 5], [0, 1, 7, 6], [1, 2, 8, 7], [2, 3, 9, 8], [3, 4, 10, 9], [4, 5, 11, 10], [5, 0, 6, 11], [6, 7, 8, 9, 10, 11]]

p8 :: Polyhedron
p8 = Polyhedron "p8" points (transformFaces points vertexes) vertexes
    where
        h = sin22_5
        points = [
            Point cos0 sin0 h,
            Point cos45 sin45 h,
            Point cos90 sin90 h,
            Point cos135 sin135 h,
            Point cos180 sin180 h,
            Point cos225 sin225 h,
            Point cos270 sin270 h,
            Point cos315 sin315 h,
            Point cos0 sin0 (-1 * h),
            Point cos45 sin45 (-1 * h),
            Point cos90 sin90 (-1 * h),
            Point cos135 sin135 (-1 * h),
            Point cos180 sin180 (-1 * h),
            Point cos225 sin225 (-1 * h),
            Point cos270 sin270 (-1 * h),
            Point cos315 sin315 (-1 * h)
            ]
        vertexes = [[0, 1, 2, 3, 4, 5, 6, 7], [0, 1, 9, 8], [1, 2, 10, 9], [2, 3, 11, 10], [3, 4, 12, 11], [4, 5, 13, 12], [5, 6, 14, 13], [6, 7, 15, 14], [7, 0, 8, 15], [8, 9, 10, 11, 12, 13, 14, 15]]

p10 :: Polyhedron
p10 = Polyhedron "p10" points (transformFaces points vertexes) vertexes
    where
        h = sin18
        points = [
            Point cos0 sin0 h,
            Point cos36 sin36 h,
            Point cos72 sin72 h,
            Point cos108 sin108 h,
            Point cos144 sin144 h,
            Point cos180 sin180 h,
            Point cos216 sin216 h,
            Point cos252 sin252 h,
            Point cos288 sin288 h,
            Point cos324 sin324 h,
            Point cos0 sin0 (-1 * h),
            Point cos36 sin36 (-1 * h),
            Point cos72 sin72 (-1 * h),
            Point cos108 sin108 (-1 * h),
            Point cos144 sin144 (-1 * h),
            Point cos180 sin180 (-1 * h),
            Point cos216 sin216 (-1 * h),
            Point cos252 sin252 (-1 * h),
            Point cos288 sin288 (-1 * h),
            Point cos324 sin324 (-1 * h)
            ]
        vertexes = [[0, 1, 2, 3, 4, 5, 6, 7, 8, 9], [0, 1, 11, 10], [1, 2, 12, 11], [2, 3, 13, 12], [3, 4, 14, 13], [4, 5, 15, 14], [5, 6, 16, 15], [6, 7, 17, 16], [7, 8, 18, 17], [8, 9, 19, 18], [9, 0, 10, 19], [10, 11, 12, 13, 14, 15, 16, 17, 18, 19]]

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
