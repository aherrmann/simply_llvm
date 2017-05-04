{-# LANGUAGE FlexibleContexts #-}

module Simply.LLVM.JIT
  ( printLLVM
  , printLLVMOpt

  , printAssembly
  , printAssemblyOpt

  , exec
  , execOpt

  , call
  , callOpt

  , optNone
  , optNoinline
  , optInline
  , optTarget
  ) where

import Protolude
import Data.String (String)
import Data.IORef

import Foreign.LibFFI

import Control.Monad.Cont

import LLVM.Target
import LLVM.Context
import LLVM.Module as Mod
import qualified LLVM.AST as AST

import LLVM.PassManager
import LLVM.Analysis

import qualified LLVM.ExecutionEngine as EE


----------------------------------------------------------------------
-- Error Handling

data JitError
  = LLVMError String
  | ProgramError String
  deriving (Show, Eq, Ord, Typeable)

instance Exception JitError


----------------------------------------------------------------------
-- The JIT

type Jit a = ContT () IO a

runJit :: Jit a -> IO ()
runJit = flip runContT (const $ pure ())

parseModule :: Context -> AST.Module -> Jit Module
parseModule ctx llvmAst = do
    m <- ContT $ withModuleFromAST ctx llvmAst
    liftIO $ verify m
    pure m

printLLVMOpt :: Optimizer -> AST.Module -> IO ()
printLLVMOpt opt llvmAst = runJit $ do
    context <- ContT withContext
    m <- parseModule context llvmAst
    _ <- liftIO $ opt context m
    s <- liftIO $ moduleLLVMAssembly m
    liftIO $ putStrLn s

printLLVM :: AST.Module -> IO ()
printLLVM = printLLVMOpt optNone

printAssemblyOpt :: Optimizer -> AST.Module -> IO ()
printAssemblyOpt opt llvmAst = runJit $ do
    context <- ContT withContext
    m <- parseModule context llvmAst
    _ <- liftIO $ opt context m
    machine <- ContT $ withHostTargetMachine
    s <- liftIO $ moduleTargetAssembly machine m
    liftIO $ putStrLn s

printAssembly :: AST.Module -> IO ()
printAssembly = printAssemblyOpt optNone

execOpt :: Optimizer -> [Int32] -> AST.Module -> IO ()
execOpt opt args llvmAst = runJit $ do
    context <- ContT withContext
    m <- parseModule context llvmAst
    _ <- liftIO $ opt context m
    engine <-
        ContT $ EE.withMCJIT context optlevel model framePtrElim fastInstr
    bin <- ContT $ EE.withModuleInEngine engine m
    mbMainfn <- liftIO $ EE.getFunction bin (AST.Name "__main")
    mainfn <- maybe (throwIO $ ProgramError "No main function") pure mbMainfn
    res <- liftIO $ callFFI mainfn retInt32 (map argInt32 args)
    liftIO $ putText $ "Result: " <> show res
  where
    optlevel = Just 0
    model = Nothing
    framePtrElim = Nothing
    fastInstr = Nothing

exec :: [Int32] -> AST.Module -> IO ()
exec = execOpt optNone

callOpt :: Optimizer -> [Int32] -> AST.Module -> IO Int32
callOpt opt args llvmAst = withRef $ \res -> runJit $ do
    context <- ContT withContext
    m <- parseModule context llvmAst
    _ <- liftIO $ opt context m
    engine <-
        ContT $ EE.withMCJIT context optlevel model framePtrElim fastInstr
    bin <- ContT $ EE.withModuleInEngine engine m
    mbMainfn <- liftIO $ EE.getFunction bin (AST.Name "__main")
    mainfn <- maybe (throwIO $ ProgramError "No main function") pure mbMainfn
    liftIO $ writeIORef res =<< callFFI mainfn retInt32 (map argInt32 args)
  where
    optlevel = Just 0
    model = Nothing
    framePtrElim = Nothing
    fastInstr = Nothing
    withRef :: (IORef Int32 -> IO ()) -> IO Int32
    withRef f = do
      ref <- newIORef 0
      f ref
      readIORef ref

call :: [Int32] -> AST.Module -> IO Int32
call = callOpt optNone


----------------------------------------------------------------------
-- Optimizers

type Optimizer = Context -> Module -> IO Bool

optNone :: Optimizer
optNone _ _ = pure False

optNoinline :: Optimizer
optNoinline _context llvmAst = withPassManager passes $ \pm ->
    runPassManager pm llvmAst
  where
    passes = defaultCuratedPassSetSpec
      { optLevel = Just 3
      , sizeLevel = Just 1
      , simplifyLibCalls = Just True
      , loopVectorize = Just True
      , superwordLevelParallelismVectorize = Just True
      }

optInline :: Optimizer
optInline _context llvmAst = withPassManager passes $ \pm ->
    runPassManager pm llvmAst
  where
    passes = defaultCuratedPassSetSpec
      { optLevel = Just 3
      , sizeLevel = Just 1
      , simplifyLibCalls = Just True
      , loopVectorize = Just True
      , superwordLevelParallelismVectorize = Just True
      , useInlinerWithThreshold = Just 225
      }

optTarget :: Optimizer
optTarget _context llvmAst = flip runContT pure $ do
    triple <- liftIO getProcessTargetTriple
    machine <- ContT $ withHostTargetMachine
    libraryInfo <- ContT $ withTargetLibraryInfo triple
    layout <- liftIO $ getTargetMachineDataLayout machine
    let passes = defaultCuratedPassSetSpec
          { optLevel = Just 3
          , sizeLevel = Just 1
          , simplifyLibCalls = Just True
          , loopVectorize = Just True
          , superwordLevelParallelismVectorize = Just True
          , useInlinerWithThreshold = Just 225
          , dataLayout = Just layout
          , targetLibraryInfo = Just libraryInfo
          , targetMachine = Just machine
          }
    pm <- ContT $ withPassManager passes
    liftIO $ runPassManager pm llvmAst
