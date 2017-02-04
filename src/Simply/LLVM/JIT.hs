{-# LANGUAGE FlexibleContexts #-}

module Simply.LLVM.JIT
  ( printLLVM
  , printLLVMOpt

  , printAssembly
  , printAssemblyOpt

  , exec
  , execOpt

  , optNone
  , optNoinline
  , optInline
  , optTarget
  ) where

import Protolude
import Data.String (String)

import Data.Int
import Data.Word
import Foreign.LibFFI

import Control.Monad.Cont
import Control.Monad.Except

import LLVM.Target
import LLVM.Context
import LLVM.CodeModel
import LLVM.Module as Mod
import qualified LLVM.AST as AST

import LLVM.PassManager
import LLVM.Transforms
import LLVM.Analysis

import qualified LLVM.ExecutionEngine as EE


----------------------------------------------------------------------
-- Error Handling

data JitError
  = LLVMError String
  | ProgramError String
  deriving (Show, Eq, Ord, Typeable)

instance Exception JitError

throwLeft :: MonadIO m => ExceptT String m a -> m a
throwLeft a = do
    errOrRes <- runExceptT a
    case errOrRes of
      Left err ->
        throwIO . LLVMError $! "LLVM Error: " <> toS err
      Right res ->
        pure res


----------------------------------------------------------------------
-- The JIT

type Jit a = ContT () IO a

runJit :: Jit a -> IO ()
runJit = flip runContT (const $ pure ())

parseModule :: Context -> AST.Module -> Jit Module
parseModule ctx mod = do
    m <- ContT $ throwLeft . withModuleFromAST ctx mod
    liftIO $ throwLeft $ verify m
    pure m

printLLVMOpt :: Optimizer -> AST.Module -> IO ()
printLLVMOpt opt mod = runJit $ do
    context <- ContT withContext
    m <- parseModule context mod
    liftIO $ opt context m
    s <- liftIO $ moduleLLVMAssembly m
    liftIO $ putStrLn s

printLLVM :: AST.Module -> IO ()
printLLVM = printLLVMOpt optNone

printAssemblyOpt :: Optimizer -> AST.Module -> IO ()
printAssemblyOpt opt mod = runJit $ do
    context <- ContT withContext
    m <- parseModule context mod
    liftIO $ opt context m
    machine <- ContT $ throwLeft . withHostTargetMachine
    s <- liftIO $ throwLeft $ moduleTargetAssembly machine m
    liftIO $ putStrLn s

printAssembly :: AST.Module -> IO ()
printAssembly = printAssemblyOpt optNone

execOpt :: Optimizer -> [Int32] -> AST.Module -> IO ()
execOpt opt args mod = runJit $ do
    context <- ContT withContext
    m <- parseModule context mod
    liftIO $ opt context m
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


----------------------------------------------------------------------
-- Optimizers

type Optimizer = Context -> Module -> IO Bool

optNone :: Optimizer
optNone _ _ = pure False

optNoinline :: Optimizer
optNoinline context mod = withPassManager passes $ \pm ->
    runPassManager pm mod
  where
    passes = defaultCuratedPassSetSpec
      { optLevel = Just 3
      , sizeLevel = Just 1
      , simplifyLibCalls = Just True
      , loopVectorize = Just True
      , superwordLevelParallelismVectorize = Just True
      }

optInline :: Optimizer
optInline context mod = withPassManager passes $ \pm ->
    runPassManager pm mod
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
optTarget context mod = flip runContT pure $ do
    triple <- liftIO getProcessTargetTriple
    machine <- ContT $ throwLeft . withHostTargetMachine
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
    liftIO $ runPassManager pm mod
