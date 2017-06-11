{-# LANGUAGE TemplateHaskell #-}

module Main (main) where

import Protolude

import Test.Hspec
import Test.Hspec.Hedgehog
import Hedgehog

import Simply.Surface.AST as Simply
import qualified Simply.Surface.Parse as Simply
import qualified Simply.Surface.Pretty as Simply
import qualified Simply.Surface.TypeCheck as Simply
import qualified Simply.Intermediate.FromSurface as Intermediate
import qualified Simply.LLVM.FromIntermediate as LLVM
import qualified Simply.LLVM.JIT as JIT

import qualified Simply.Examples as Example

import qualified Simply.Surface.Gen.IllTyped as IllTyped
import qualified Simply.Surface.Gen.WellTyped as WellTyped


prop_illTyped_prettyPrint_parse_roundTrip :: Property
prop_illTyped_prettyPrint_parse_roundTrip = property $ do
  ast <- forAll IllTyped.genProgram
  tripping ast Simply.prettyPlainTextFast Simply.parseText

prop_wellTyped_prettyPrint_parse_roundTrip :: Property
prop_wellTyped_prettyPrint_parse_roundTrip = property $ do
  ast <- forAll WellTyped.genProgram
  tripping ast Simply.prettyPlainTextFast Simply.parseText

prop_wellTyped_typeChecks :: Property
prop_wellTyped_typeChecks = property $ do
  program <- forAll WellTyped.genProgram
  void $ liftEither $ Simply.typeCheck program

prop_wellTyped_compiles :: Property
prop_wellTyped_compiles = property $ do
  program <- forAll WellTyped.genProgram
  checked <- liftEither $ Simply.typeCheck program
  let intermediate = Intermediate.fromSurface checked
  annotateShow intermediate
  annotate $ toS $ JIT.ppllvm $ LLVM.fromIntermediate intermediate
  result <- liftIO $ verifyProgram program
    `catch` \ e -> pure $ Left $ show (e :: SomeException)
  case result of
    Right () -> success
    Left err -> do
      footnote (toS err)
      failure


withProgram :: Simply.Program -> (([Int32] -> IO Int32) -> IO a) -> IO a
withProgram program user = do
  checked <- Simply.typeCheck' program
  let
    intermediate = Intermediate.fromSurface checked
    llvm = LLVM.fromIntermediate intermediate
  JIT.withExec llvm user

run0 :: Simply.Program -> IO Int32
run0 prog = withProgram prog apply0
  where apply0 f = f []

run1 :: Simply.Program -> [Int32] -> IO [Int32]
run1 prog args = withProgram prog $ for args . apply1
  where apply1 f x = f [x]


verifyProgram :: Simply.Program -> IO (Either Text ())
verifyProgram program = do
  checked <- Simply.typeCheck' program
  let
    intermediate = Intermediate.fromSurface checked
    llvm = LLVM.fromIntermediate intermediate
  JIT.verifyModule llvm


factorial :: Integral a => a -> a
factorial = go 1
  where
    go acc 0 = acc
    go acc n = go (acc * n) (n - 1)


main :: IO ()
main = hspec $ do

  describeGroup $$(discover)

  let
    hasArgc argc = \case
      Right prog -> argc == Simply.programMainNumArgs prog
      _ -> False

  describe "Example.ex01a_factorial" $ do
    let prog = Example.ex01a_factorial
    it "type-checks" $
      Simply.typeCheck prog `shouldSatisfy` hasArgc 0
    it "verifies" $
      verifyProgram prog `shouldReturn` Right ()
    it "compiles and runs" $
      run0 prog `shouldReturn` factorial 5

  describe "Example.ex01b_factorial" $ do
    let prog = Example.ex01b_factorial
    it "type-checks" $
      Simply.typeCheck prog `shouldSatisfy` hasArgc 1
    it "verifies" $
      verifyProgram prog `shouldReturn` Right ()
    it "compiles and runs" $
      prog `run1` [0..7] `shouldReturn` map factorial [0..7]

  describe "Example.ex01c_factorial" $ do
    let prog = Example.ex01c_factorial
    it "type-checks" $
      Simply.typeCheck prog `shouldSatisfy` hasArgc 0
    it "verifies" $
      verifyProgram prog `shouldReturn` Right ()
    it "compiles and runs" $
      run0 prog `shouldReturn` factorial 5

  describe "Example.ex01d_factorial" $ do
    let prog = Example.ex01d_factorial
    it "type-checks" $
      Simply.typeCheck prog `shouldSatisfy` hasArgc 1
    it "verifies" $
      verifyProgram prog `shouldReturn` Right ()
    it "compiles and runs" $
      prog `run1` [0..7] `shouldReturn` map factorial [0..7]

  describe "Example.ex02a_higher_order" $ do
    let prog = Example.ex02a_higher_order
    it "type-checks" $
      Simply.typeCheck prog `shouldSatisfy` hasArgc 1
    it "verifies" $
      verifyProgram prog `shouldReturn` Right ()
    it "compiles and runs" $
      run0 prog `shouldReturn` 7

  describe "Example.ex02b_higher_order" $ do
    let prog = Example.ex02b_higher_order
    it "type-checks" $
      Simply.typeCheck prog `shouldSatisfy` hasArgc 1
    it "verifies" $
      verifyProgram prog `shouldReturn` Right ()
    it "compiles and runs" $
      prog `run1` [0..7] `shouldReturn` map (+3) [0..7]

  describe "Example.ex03_factorial_fix" $ do
    let prog = Example.ex03_factorial_fix
    it "type-checks" $
      Simply.typeCheck prog `shouldSatisfy` hasArgc 1
    it "verifies" $
      verifyProgram prog `shouldReturn` Right ()
    it "compiles and runs" $
      prog `run1` [0..7] `shouldReturn` map factorial [0..7]
