{-# LANGUAGE DeriveGeneric #-}

module Main where

import Triangle
import Point
import Face
import Polyhedron
import ConvexPolyhedron
import Character
import AlgUtil
import Data.Aeson
import GHC.Generics
import qualified Data.ByteString.Lazy.Char8 as BL

{-
整面凸多面体であるということ

点の条件
* 同じ点がないこと
* 同一直線上に3点ないこと
* 同一平面上に4点ないこと(<- これだけ見ればいい)

各面の条件
* 3点以上から構成されている
* 正多角形である
* 一辺の長さが定数値

面全体の条件
* どの点もどこかの面の頂点に含まれている
* 一つの辺には2つの面だけ接する
* 凸性(面を構成しない点と平面の行列式の符号が全て同じ)
-}

{-
求めたいもの

* 1辺の長さ
* 頂点数
* 辺数
* 面数
* 表面積
* 表面積(1辺1)
* 体積
* 体積(1辺1)
* 頂点形状と余角
* 辺形状と2面角 <- これは最初は求めない
-}

data OutputData = OutputData
    { a :: [Int]
    , b :: Int
    } deriving (Generic, Show)

instance ToJSON OutputData

main :: IO ()
-- main = print $ (tan11_25 - sin11_25 / cos11_25)
-- main = print $ isSamePlain (Point sin15 cos30 43) (Point sin30 cos15 34) (Point sin11_25 cos15 2) (Point sin15 cos9 3)
-- main = print $ isSamePlain (Point 1 1 1) (Point 2 2 2) (Point 3 3 3) (Point 4 4 4)
-- main = print $ isRegularFace $ Face [(Point 0 0 0), (Point 0 0 1), (Point 0 1 1), (Point 0 1 0)]
-- main = print $ isConvexPolyhedron r1
-- main = print $ toValue $ volume (pfaces r1)
main = BL.putStrLn $ encode $ OutputData [1, 3] 6
-- main = print $ toValue 1
-- main = print $ pfaces r1

