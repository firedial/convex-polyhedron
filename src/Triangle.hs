module Triangle where

import AlgebraicNum.AlgReal
import AlgUtil

sin0 :: AlgReal
sin0 = 0
cos0 :: AlgReal
cos0 = 1
tan0 :: AlgReal
tan0 = 0

sin9 :: AlgReal
sin9 = toLoad [-19,0,32,0,224,0,-512,0,256] 3
cos9 :: AlgReal
cos9 = toLoad [-19,0,32,0,224,0,-512,0,256] 4
tan9 :: AlgReal
tan9 = sin9 / cos9

sin11_25 :: AlgReal
sin11_25 = toLoad [1,0,-32,0,160,0,-256,0,128] 4
cos11_25 :: AlgReal
cos11_25 = toLoad [1,0,-32,0,160,0,-256,0,128] 7
tan11_25 :: AlgReal
tan11_25 = sin11_25 / cos11_25

sin15 :: AlgReal
sin15 = toLoad [1,0,-16,0,16] 2
cos15 :: AlgReal
cos15 = toLoad [1,0,-16,0,16] 3
tan15 :: AlgReal
tan15 = sin15 / cos15

sin18 :: AlgReal
sin18 = toLoad [-1,2,4] 1
cos18 :: AlgReal
cos18 = toLoad [5,0,-20,0,16] 3
tan18 :: AlgReal
tan18 = sin18 / cos18

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

sin36 :: AlgReal
sin36 = 2 * sin18 * cos18
cos36 :: AlgReal
cos36 = 2 * cos18 * cos18 - 1
tan36 :: AlgReal
tan36 = sin36 / cos36

sin45 :: AlgReal
sin45 = toLoad [-1,0,2] 1
cos45 :: AlgReal
cos45 = sin45
tan45 :: AlgReal
tan45 = sin45 / cos45

sin54 :: AlgReal
sin54 = cos36
cos54 :: AlgReal
cos54 = sin36
tan54 :: AlgReal
tan54 = sin54 / cos54

sin60 :: AlgReal
sin60 = cos30
cos60 :: AlgReal
cos60 = sin30
tan60 :: AlgReal
tan60 = sin60 / cos60

sin67_5 :: AlgReal
sin67_5 = cos22_5
cos67_5 :: AlgReal
cos67_5 = sin22_5
tan67_5 :: AlgReal
tan67_5 = sin67_5 / cos67_5

sin72 :: AlgReal
sin72 = cos18
cos72 :: AlgReal
cos72 = sin18
tan72 :: AlgReal
tan72 = sin72 / cos72

sin90 :: AlgReal
sin90 = 1
cos90 :: AlgReal
cos90 = 0

sin108 :: AlgReal
sin108 = sin72
cos108 :: AlgReal
cos108 = -1 * cos72
tan108 :: AlgReal
tan108 = sin108 / cos108

sin112_5 :: AlgReal
sin112_5 = sin67_5
cos112_5 :: AlgReal
cos112_5 = -1 * cos67_5
tan112_5 :: AlgReal
tan112_5 = sin112_5 / cos112_5

sin120 :: AlgReal
sin120 = sin60
cos120 :: AlgReal
cos120 = -1 * cos60
tan120 :: AlgReal
tan120 = sin120 / cos120

sin126 :: AlgReal
sin126 = sin54
cos126 :: AlgReal
cos126 = -1 * cos54
tan126 :: AlgReal
tan126 = sin126 / cos126

sin135 :: AlgReal
sin135 = sin45
cos135 :: AlgReal
cos135 = -1 * cos45
tan135 :: AlgReal
tan135 = sin135 / cos135

sin144 :: AlgReal
sin144 = sin36
cos144 :: AlgReal
cos144 = -1 * cos36
tan144 :: AlgReal
tan144 = sin144 / cos144

sin150 :: AlgReal
sin150 = sin30
cos150 :: AlgReal
cos150 = -1 * cos30
tan150 :: AlgReal
tan150 = sin150 / cos150

sin157_5 :: AlgReal
sin157_5 = sin22_5
cos157_5 :: AlgReal
cos157_5 = -1 * cos22_5
tan157_5 :: AlgReal
tan157_5 = sin157_5 / cos157_5

sin162 :: AlgReal
sin162 = sin18
cos162 :: AlgReal
cos162 = -1 * cos18
tan162 :: AlgReal
tan162 = sin162 / cos162

sin180 :: AlgReal
sin180 = 0
cos180 :: AlgReal
cos180 = -1
tan180 :: AlgReal
tan180 = 0

sin198 :: AlgReal
sin198 = -1 * sin18
cos198 :: AlgReal
cos198 = -1 * cos18
tan198 :: AlgReal
tan198 = sin198 / cos198

sin202_5 :: AlgReal
sin202_5 = -1 * sin22_5
cos202_5 :: AlgReal
cos202_5 = -1 * cos22_5
tan202_5 :: AlgReal
tan202_5 = sin202_5 / cos202_5

sin210 :: AlgReal
sin210 = -1 * sin30
cos210 :: AlgReal
cos210 = -1 * cos30
tan210 :: AlgReal
tan210 = sin210 / cos210

sin216 :: AlgReal
sin216 = -1 * sin36
cos216 :: AlgReal
cos216 = -1 * cos36
tan216 :: AlgReal
tan216 = sin216 / cos216

sin225 :: AlgReal
sin225 = -1 * sin45
cos225 :: AlgReal
cos225 = -1 * cos45
tan225 :: AlgReal
tan225 = sin225 / cos225

sin234 :: AlgReal
sin234 = -1 * sin54
cos234 :: AlgReal
cos234 = -1 * cos54
tan234 :: AlgReal
tan234 = sin234 / cos234

sin240 :: AlgReal
sin240 = -1 * sin120
cos240 :: AlgReal
cos240 = cos120
tan240 :: AlgReal
tan240 = sin240 / cos240

sin247_5 :: AlgReal
sin247_5 = -1 * sin67_5
cos247_5 :: AlgReal
cos247_5 = -1 * cos67_5
tan247_5 :: AlgReal
tan247_5 = sin247_5 / cos247_5

sin252 :: AlgReal
sin252 = -1 * sin72
cos252 :: AlgReal
cos252 = -1 * cos72
tan252 :: AlgReal
tan252 = sin252 / cos252

sin270 :: AlgReal
sin270 = -1
cos270 :: AlgReal
cos270 = 0

sin288 :: AlgReal
sin288 = -1 * sin72
cos288 :: AlgReal
cos288 = cos72
tan288 :: AlgReal
tan288 = sin288 / cos288

sin292_5 :: AlgReal
sin292_5 = sin247_5
cos292_5 :: AlgReal
cos292_5 = -1 * cos247_5
tan292_5 :: AlgReal
tan292_5 = sin292_5 / cos292_5

sin300 :: AlgReal
sin300 = -1 * sin60
cos300 :: AlgReal
cos300 = cos60
tan300 :: AlgReal
tan300 = sin300 / cos300

sin306 :: AlgReal
sin306 = -1 * sin54
cos306 :: AlgReal
cos306 = cos54
tan306 :: AlgReal
tan306 = sin306 / cos306

sin315 :: AlgReal
sin315 = -1 * sin45
cos315 :: AlgReal
cos315 = cos45
tan315 :: AlgReal
tan315 = sin315 / cos315

sin324 :: AlgReal
sin324 = -1 * sin36
cos324 :: AlgReal
cos324 = cos36
tan324 :: AlgReal
tan324 = sin324 / cos324

sin330 :: AlgReal
sin330 = -1 * sin30
cos330 :: AlgReal
cos330 = cos30
tan330 :: AlgReal
tan330 = sin330 / cos330

sin337_5 :: AlgReal
sin337_5 = -1 * sin22_5
cos337_5 :: AlgReal
cos337_5 = cos22_5
tan337_5 :: AlgReal
tan337_5 = sin337_5 / cos337_5

sin342 :: AlgReal
sin342 = -1 * sin18
cos342 :: AlgReal
cos342 = cos18
tan342 :: AlgReal
tan342 = sin342 / cos342
