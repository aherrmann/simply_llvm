{-# LANGUAGE FlexibleContexts #-}

module Simply.LLVM.JIT
  ( printLLVM
  , printLLVMOpt

  , printAssembly
  , printAssemblyOpt

  , exec
  , execOpt

  , withExec
  , withExecOpt

  , optNone
  , optNoinline
  , optInline
  , optTarget

  , verifyModule
  , VerifyException (..)
  ) where

import Protolude

import Control.Monad.Managed
  (Managed, MonadManaged, managed, runManaged, using, with)
import Foreign.LibFFI (argInt32, callFFI, retInt32)

import qualified LLVM.AST as AST

import LLVM.Analysis (verify)
import LLVM.AST.DataLayout (DataLayout)
import LLVM.Context (Context, withContext)
import LLVM.Exception (VerifyException (..))
import LLVM.ExecutionEngine (getFunction, withMCJIT, withModuleInEngine)
import LLVM.Module
  (Module, withModuleFromAST, moduleLLVMAssembly, moduleTargetAssembly)
import LLVM.PassManager
  ( PassManager, PassSetSpec (..)
  , defaultCuratedPassSetSpec, runPassManager, withPassManager
  )
import LLVM.Target
  ( TargetLibraryInfo, TargetMachine
  , getTargetMachineDataLayout, getProcessTargetTriple
  , withHostTargetMachine, withTargetLibraryInfo
  )


----------------------------------------------------------------------
-- IO Interface for Convenience

printLLVM :: AST.Module -> IO ()
printLLVM = printLLVMOpt optNone

printLLVMOpt :: Optimizer -> AST.Module -> IO ()
printLLVMOpt opt ast = runManaged . runJit $ do
  m <- moduleFromAST ast
  opt m
  s <- llvmIRFromModule m
  liftIO $ putStrLn s

printAssembly :: AST.Module -> IO ()
printAssembly = printAssemblyOpt optNone

printAssemblyOpt :: Optimizer -> AST.Module -> IO ()
printAssemblyOpt opt ast = runManaged . runJit $ do
  m <- moduleFromAST ast
  opt m
  s <- assemblyFromModule m
  liftIO $ putStrLn s

exec :: [Int32] -> AST.Module -> IO Int32
exec = execOpt optNone

execOpt :: Optimizer -> [Int32] -> AST.Module -> IO Int32
execOpt opt args ast = withExecOpt opt ast (\prog -> prog args)

withExec :: AST.Module -> (([Int32] -> IO Int32) -> IO a) -> IO a
withExec = withExecOpt optNone

withExecOpt
  :: Optimizer
  -> AST.Module
  -> (([Int32] -> IO Int32) -> IO a)
  -> IO a
withExecOpt opt ast = withJit $ do
  m <- moduleFromAST ast
  opt m
  compile m

verifyModule :: AST.Module -> IO (Either VerifyException ())
verifyModule ast = withJit (moduleFromAST ast) (liftIO . try . verify)


----------------------------------------------------------------------
-- Optimizers

type Optimizer = Module -> Jit ()

optNone :: Optimizer
optNone _ = pure ()

passOptNoinline :: PassSetSpec
passOptNoinline = defaultCuratedPassSetSpec
  { optLevel = Just 3
  , sizeLevel = Just 1
  , simplifyLibCalls = Just True
  , loopVectorize = Just True
  , superwordLevelParallelismVectorize = Just True
  }

optNoinline :: Optimizer
optNoinline m = do
  pm <- passManager passOptNoinline
  void . liftIO $ runPassManager pm m

passOptInline :: PassSetSpec
passOptInline = passOptNoinline
  { useInlinerWithThreshold = Just 225 }

optInline :: Optimizer
optInline m = do
  pm <- passManager passOptInline
  void . liftIO $ runPassManager pm m

passOptTarget :: DataLayout -> TargetLibraryInfo -> TargetMachine -> PassSetSpec
passOptTarget layout libraryInfo machine = passOptInline
  { dataLayout = Just layout
  , targetLibraryInfo = Just libraryInfo
  , targetMachine = Just machine
  }

optTarget :: Optimizer
optTarget m = do
  triple <- liftIO getProcessTargetTriple
  machine <- using $ managed withHostTargetMachine
  libraryInfo <- using $ managed (withTargetLibraryInfo triple)
  layout <- liftIO $ getTargetMachineDataLayout machine
  pm <- passManager (passOptTarget layout libraryInfo machine)
  void . liftIO $ runPassManager pm m

passManager :: PassSetSpec -> Jit PassManager
passManager passes =
  using $ managed (withPassManager passes)


----------------------------------------------------------------------
-- Error Handling

data JitError
  = MissingEntryPoint
  deriving (Show, Eq, Ord, Typeable)

instance Exception JitError


----------------------------------------------------------------------
-- The JIT

newtype Jit a = Jit (ReaderT Context Managed a)
  deriving
    ( Functor, Applicative, Monad
    , MonadReader Context, MonadIO, MonadManaged
    )

runJit :: Jit a -> Managed a
runJit (Jit m) = do
  ctx <- using $ managed withContext
  runReaderT m ctx

withJit :: Jit a -> (a -> IO r) -> IO r
withJit = with . runJit

moduleFromAST :: AST.Module -> Jit Module
moduleFromAST ast = do
  ctx <- ask
  using $ managed (withModuleFromAST ctx ast)

llvmIRFromModule :: Module -> Jit ByteString
llvmIRFromModule = liftIO . moduleLLVMAssembly

assemblyFromModule :: Module -> Jit ByteString
assemblyFromModule m = do
  machine <- using $ managed withHostTargetMachine
  liftIO $ moduleTargetAssembly machine m

compile :: Module -> Jit ([Int32] -> IO Int32)
compile m = do
  let optlevel = Just 0
      model = Nothing
      framePtrElim = Nothing
      fastInstr = Nothing
  ctx <- ask
  engine <- using $ managed (withMCJIT ctx optlevel model framePtrElim fastInstr)
  bin <- using $ managed (withModuleInEngine engine m)
  mbMainFun <- liftIO $ getFunction bin (AST.Name "main")
  mainFun <- maybe (throwIO $ MissingEntryPoint) pure mbMainFun
  pure $! \ args -> callFFI mainFun retInt32 (map argInt32 args)
