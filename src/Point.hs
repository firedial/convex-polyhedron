module Point where

import AlgebraicNum.AlgReal

data Point = Point {x :: AlgReal, y :: AlgReal, z :: AlgReal} deriving (Show, Eq)

instance Num Point where
    (Point x1 y1 z1) + (Point x2 y2 z2) = Point (x1 + x2) (y1 + y2) (z1 + z2)
    p1 - p2 = p1 + (-p2)
    (*) _ _ = error "not computable"
    negate (Point x y z) = Point (-x) (-y) (-z)
    abs _ = error "not computable"
    signum _ = error "not computable"
    fromInteger _ = error "not computable"

norm :: Point -> AlgReal
norm p = sqrtA $ norm2 p

norm2 :: Point -> AlgReal
norm2 p = (x p) * (x p) + (y p) * (y p) + (z p) * (z p)

distance2 :: Point -> Point -> AlgReal
distance2 p1 p2 = norm2 (p2 - p1)

det :: Point -> Point -> Point -> AlgReal
det v1 v2 v3 = (x v1) * (y v2) * (z v3) + (y v1) * (z v2) * (x v3) + (z v1) * (x v2) * (y v3) - (z v1) * (y v2) * (x v3) - (y v1) * (x v2) * (z v3) - (x v1) * (z v2) * (y v3)

isSamePlain :: Point -> Point -> Point -> Point -> Bool
isSamePlain p1 p2 p3 p4 = (det (p2 - p1) (p3 - p1) (p4 - p1)) == 0

innerProduct :: Point -> Point -> AlgReal
innerProduct v1 v2 = (x v1) * (x v2) + (y v1) * (y v2) + (z v1) * (z v2)

cosAngle :: Point -> Point -> Point -> AlgReal
cosAngle p1 p2 p3 = (innerProduct v1 v3) / ((norm v1) * (norm v3))
    where
        v1 = p1 - p2
        v3 = p3 - p2

