module Face where

import AlgebraicNum.AlgReal
import Point

data Face = Face {points :: [Point]}

isSamePlainFaces :: Point -> Point -> Point -> [Point] -> Bool
isSamePlainFaces _ _ _ [] = True
isSamePlainFaces p1 p2 p3 (p:ps) = (isSamePlain p1 p2 p3 p) && (isSamePlainFaces p1 p2 p3 ps)

isSameEdgeFaces :: AlgReal -> Point -> [Point] -> Bool
isSameEdgeFaces _ _ [] = True
isSameEdgeFaces d2 p1 (p:ps)
    | d2 == distance2 p1 p = isSameEdgeFaces d2 p ps
    | otherwise = False

isSameAngleFaces :: AlgReal -> Point -> Point -> [Point] -> Bool
isSameAngleFaces _ _ _ [] = True
isSameAngleFaces angle p1 p2 (p3:ps)
    | angle == cosAngle p1 p2 p3 = isSameAngleFaces angle p2 p3 ps
    | otherwise = False

isRegularFace :: Face -> Bool
isRegularFace f
    = d2 /= 0
    && length ps >= 3
    && (isSamePlainFaces pp1 pp2 pp3 pps)
    && (isSameEdgeFaces d2 pf pfs)
    && (anglen == angle1) && isSameAngleFaces angle1 pa1 pa2 pas
    where
        ps = points f
        (pp1:pp2:pp3:pps) = ps

        (pf:pfs) = ps
        d2 = distance2 (head ps) (last ps)

        (pan:pan1:_) = reverse ps
        (pa1:pa2:pas) = ps
        anglen = cosAngle pan1 pan pa1
        angle1 = cosAngle pan pa1 pa2
