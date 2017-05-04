import Protolude

import Test.Hspec

import qualified Simply.AST.Simply as Simply
import qualified Simply.TypeCheck.Simply as Simply
import qualified Simply.Transform.Simply2IR as Simply2IR
import qualified Simply.Transform.IR2LLVM as IR2LLVM
import qualified Simply.LLVM.JIT as JIT

import qualified Simply.Examples.Simply as Example


withProgram :: Simply.Program -> (([Int32] -> IO Int32) -> IO a) -> IO a
withProgram prog user = do
  let
    ir = Simply2IR.transform prog
    llvm = IR2LLVM.transform ir
    run args = JIT.call args llvm
  user run

run0 :: Simply.Program -> IO Int32
run0 prog = withProgram prog $ \ p -> p []

runOn1 :: Simply.Program -> [Int32] -> IO [Int32]
runOn1 prog args = withProgram prog $ \ p -> for args (\n -> p [n])


factorial :: Integral a => a -> a
factorial = go 1
  where
    go acc 0 = acc
    go acc n = go (acc * n) (n - 1)


main :: IO ()
main = hspec $ do

  describe "Example.ex01a_factorial" $ do
    it "type-checks" $ do
      Simply.checkProgram Example.ex01a_factorial `shouldBe` Right ()
    it "works" $ do
      run0 Example.ex01a_factorial `shouldReturn` factorial 5

  describe "Example.ex01b_factorial" $ do
    it "type-checks" $ do
      Simply.checkProgram Example.ex01b_factorial `shouldBe` Right ()
    it "works" $ do
      Example.ex01b_factorial `runOn1` [0..7] `shouldReturn` map factorial [0..7]

  describe "Example.ex01c_factorial" $ do
    it "type-checks" $ do
      Simply.checkProgram Example.ex01c_factorial `shouldBe` Right ()
    it "works" $ do
      run0 Example.ex01c_factorial `shouldReturn` factorial 5

  describe "Example.ex01d_factorial" $ do
    it "type-checks" $ do
      Simply.checkProgram Example.ex01d_factorial `shouldBe` Right ()
    it "works" $ do
      Example.ex01d_factorial `runOn1` [0..7] `shouldReturn` map factorial [0..7]

  describe "Example.ex02a_higher_order" $ do
    it "type-checks" $ do
      Simply.checkProgram Example.ex02a_higher_order `shouldBe` Right ()
    it "works" $ do
      run0 Example.ex02a_higher_order `shouldReturn` 7

  describe "Example.ex02b_higher_order" $ do
    it "type-checks" $ do
      Simply.checkProgram Example.ex02b_higher_order `shouldBe` Right ()
    it "works" $ do
      Example.ex02b_higher_order `runOn1` [0..7] `shouldReturn` map (+3) [0..7]

  describe "Example.ex03_factorial_fix" $ do
    it "type-checks" $ do
      Simply.checkProgram Example.ex03_factorial_fix `shouldBe` Right ()
    it "works" $ do
      Example.ex03_factorial_fix `runOn1` [0..7] `shouldReturn` map factorial [0..7]
