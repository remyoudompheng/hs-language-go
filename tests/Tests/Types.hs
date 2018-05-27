-- |
-- Module      : Tests.Types
-- Copyright   : (c) 2013 Rémy Oudompheng
-- License     : GPLv3 (see COPYING)
-- 
-- This module provides tests for Go type system rules.

module Tests.Types (testsTypes) where

import Test.HUnit

import qualified Data.Map as M

import Language.Go.Analysis.Types

testAssignableTo :: Type -> Type -> Bool -> Test
testAssignableTo t1 t2 t = TestLabel desc $ TestCase $ assertEqual desc t (assignableTo t1 t2)
  where desc = "test that " ++ typeString t1 ++ isornot ++ "assignable to " ++ typeString t2
        isornot = if t then " is " else " is not "

-- Types defined is test/assign1.go in Go tests.
--   A [10]int
--   B []int
--   C chan int
--   F func() int
--   I interface {
--         m() int
--   }
--   M map[int]int
--   P *int
--   S struct {
--         X int
--   }

-- underlying types.
uInt = NumericType GoInt
uA = ArrayType 10 nInt
uB = SliceType nInt
uC = ChanType ChanBi nInt
uF = FuncType (Signature [] [nInt] False)
uI = IfaceType (M.singleton "m" (Signature [] [nInt] False))
uM = MapType nInt nInt
uP = PtrType nInt
uS = StructType [NamedField "X" nInt Nothing]

-- named types
nInt = NamedType "" "int" uInt []
nA = NamedType "" "A" uA []
nB = NamedType "" "B" uB []
nC = NamedType "" "C" uC []
nF = NamedType "" "F" uF []
nI = NamedType "" "I" uI []
nM = NamedType "" "M" uM []
nP = NamedType "" "P" uP []
nS = NamedType "" "S" uS []

-- named types copy
nA1 = NamedType "" "A1" uA []
nB1 = NamedType "" "B1" uB []
nC1 = NamedType "" "C1" uC []
nF1 = NamedType "" "F1" uF []
nI1 = NamedType "" "I1" uI []
nM1 = NamedType "" "M1" uM []
nP1 = NamedType "" "P1" uP []
nS1 = NamedType "" "S1" uS []

testsTypes = testA
  ++ testB
  ++ testC
  ++ testF
  ++ testI
  ++ testM
  ++ testP
  ++ testS

-- tests from assign1
testA =
  [ testAssignableTo nA  uA  True
  , testAssignableTo nA1 uA  True
  , testAssignableTo uA  nA  True
  , testAssignableTo nA1 nA  False
  , testAssignableTo uA  nA1 True
  , testAssignableTo nA  nA1 False
  ]

testB =
  [ testAssignableTo nB  uB  True
  , testAssignableTo nB1 uB  True
  , testAssignableTo uB  nB  True
  , testAssignableTo nB1 nB  False
  , testAssignableTo uB  nB1 True
  , testAssignableTo nB  nB1 False
  ]

testC =
  [ testAssignableTo nC  uC  True
  , testAssignableTo nC1 uC  True
  , testAssignableTo uC  nC  True
  , testAssignableTo nC1 nC  False
  , testAssignableTo uC  nC1 True
  , testAssignableTo nC  nC1 False
  ]

testF =
  [ testAssignableTo nF  uF  True
  , testAssignableTo nF1 uF  True
  , testAssignableTo uF  nF  True
  , testAssignableTo nF1 nF  False
  , testAssignableTo uF  nF1 True
  , testAssignableTo nF  nF1 False
  ]

testI =
  [ testAssignableTo nI  uI  True
  , testAssignableTo nI1 uI  True
  , testAssignableTo uI  nI  True
  , testAssignableTo nI1 nI  True
  , testAssignableTo uI  nI1 True
  , testAssignableTo nI  nI1 True
  ]

testM =
  [ testAssignableTo nM  uM  True
  , testAssignableTo nM1 uM  True
  , testAssignableTo uM  nM  True
  , testAssignableTo nM1 nM  False
  , testAssignableTo uM  nM1 True
  , testAssignableTo nM  nM1 False
  ]

testP =
  [ testAssignableTo nP  uP  True
  , testAssignableTo nP1 uP  True
  , testAssignableTo uP  nP  True
  , testAssignableTo nP1 nP  False
  , testAssignableTo uP  nP1 True
  , testAssignableTo nP  nP1 False
  ]

testS =
  [ testAssignableTo nS  uS  True
  , testAssignableTo nS1 uS  True
  , testAssignableTo uS  nS  True
  , testAssignableTo nS1 nS  False
  , testAssignableTo uS  nS1 True
  , testAssignableTo nS  nS1 False
  ]


