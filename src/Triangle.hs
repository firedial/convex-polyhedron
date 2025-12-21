module Triangle where

import AlgebraicNum.AlgReal
import AlgUtil

sin0 :: AlgReal
sin0 = 0
cos0 :: AlgReal
cos0 = 1
tan0 :: AlgReal
tan0 = 0

sin11_25 :: AlgReal
sin11_25 = toLoad [1,0,-32,0,160,0,-256,0,128] 5
cos11_25 :: AlgReal
cos11_25 = toLoad [1,0,-32,0,160,0,-256,0,128] 6
tan11_25 :: AlgReal
tan11_25 = sin11_25 / cos11_25

sin18 :: AlgReal
sin18 = toLoad [-1,2,4] 1
cos18 :: AlgReal
cos18 = toLoad [5,0,-20,0,16] 3
tan18 :: AlgReal
tan18 = sin18 / cos18

sin15 :: AlgReal
sin15 = toLoad [1,0,-16,0,16] 2
cos15 :: AlgReal
cos15 = toLoad [1,0,-16,0,16] 3
tan15 :: AlgReal
tan15 = sin15 / cos15

sin9 :: AlgReal
sin9 = toLoad [-19,0,32,0,224,0,-512,0,256] 3
cos9 :: AlgReal
cos9 = toLoad [-19,0,32,0,224,0,-512,0,256] 4
tan9 :: AlgReal
tan9 = sin9 / cos9

sin22_5 :: AlgReal
sin22_5 = 2 * sin11_25 * cos11_25
cos22_5 :: AlgReal
cos22_5 = 2 * cos11_25 ^ 2 - 1
tan22_5 :: AlgReal
tan22_5 = sin22_5 / cos22_5

sin30 :: AlgReal
sin30 = toLoad [-1,2] 0
cos30 :: AlgReal
cos30 = toLoad [-3,0,4] 1
tan30 :: AlgReal
tan30 = sin30 / cos30

sin60 :: AlgReal
sin60 = cos30
cos60 :: AlgReal
cos60 = sin30
tan60 :: AlgReal
tan60 = sin60 / cos60

sin120 :: AlgReal
sin120 = sin60
cos120 :: AlgReal
cos120 = -1 * cos60
tan120 :: AlgReal
tan120 = sin120 / cos120

sin240 :: AlgReal
sin240 = -1 * sin120
cos240 :: AlgReal
cos240 = cos120
tan240 :: AlgReal
tan240 = sin240 / cos240
