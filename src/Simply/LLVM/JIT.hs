module Simply.LLVM.JIT
  ( printLLVM
  , printLLVMOpt

  , printAssembly
  , printAssemblyOpt

  , exec
  , execOpt

  , withExec
  , withExecOpt

  , writeObjectFile
  , writeObjectFileOpt

  , buildExecutableOpt
  , buildExecutable

  , optNone
  , optNoinline
  , optInline
  , optTarget

  , verifyModule

  , writeModuleFile
  , ppllvm
  ) where

import Protolude

import Control.Exception (Handler (..))
import Control.Monad.Managed
  (Managed, MonadManaged, managed, runManaged, using, with)
import qualified Data.ByteString.Lazy as LazyByteString
import qualified Data.Text.Lazy.Encoding as LazyText
import qualified Data.Text.Lazy.IO as LazyText
import Foreign.LibFFI (argInt32, callFFI, retInt32)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.Process (callProcess)

import qualified LLVM.AST as AST

import LLVM.Analysis (verify)
import LLVM.AST.DataLayout (DataLayout)
import LLVM.Context (Context, withContext)
import LLVM.Exception (ParseFailureException (..), VerifyException (..))
import LLVM.ExecutionEngine (getFunction, withMCJIT, withModuleInEngine)
import LLVM.Internal.Module (withModuleFromLLVMAssembly)
import LLVM.Module
  ( File (..), Module
  , withModuleFromAST, moduleLLVMAssembly, moduleTargetAssembly
  , writeObjectToFile
  )
import LLVM.PassManager
  ( PassManager, PassSetSpec (..)
  , defaultCuratedPassSetSpec, runPassManager, withPassManager
  )
import LLVM.Pretty (ppllvm)
import LLVM.Target
  ( TargetLibraryInfo, TargetMachine
  , getTargetMachineDataLayout, getProcessTargetTriple
  , withHostTargetMachine, withTargetLibraryInfo
  )

import Simply.LLVM.FromIntermediate (mangle)

import Paths_simply_llvm (getDataFileName)


----------------------------------------------------------------------
-- IO Interface for Convenience

printLLVM :: AST.Module -> IO ()
printLLVM = printLLVMOpt optNone

printLLVMOpt :: Optimizer -> AST.Module -> IO ()
printLLVMOpt opt ast = runJIT $ do
  m <- moduleFromAST ast
  opt m
  s <- llvmIRFromModule m
  liftIO $ putStrLn s

printAssembly :: AST.Module -> IO ()
printAssembly = printAssemblyOpt optNone

printAssemblyOpt :: Optimizer -> AST.Module -> IO ()
printAssemblyOpt opt ast = runJIT $ do
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
withExecOpt opt ast = withJIT $ do
  m <- moduleFromAST ast
  opt m
  compile m

verifyModule :: AST.Module -> IO (Either Text ())
verifyModule ast = (Right <$> runJIT parseAndVerify) `catches`
  [ Handler $ \ (ParseFailureException err) -> pure $ Left $! toS err
  , Handler $ \ (VerifyException err) -> pure $ Left $! toS err
  ]
  where
    parseAndVerify = do
      ctx <- ask
      let llvm = LazyByteString.toStrict . LazyText.encodeUtf8 $ ppllvm ast
      m <- using $ managed $ withModuleFromLLVMAssembly ctx llvm
      liftIO $ verify m

writeModuleFile :: FilePath -> AST.Module -> IO ()
writeModuleFile path = LazyText.writeFile path . ppllvm

writeObjectFileOpt :: Optimizer -> FilePath -> AST.Module -> IO ()
writeObjectFileOpt opt path ast = runJIT $ do
  m <- moduleFromAST ast
  opt m
  machine <- using $ managed withHostTargetMachine
  liftIO $ writeObjectToFile machine (File path) m

writeObjectFile :: FilePath -> AST.Module -> IO ()
writeObjectFile = writeObjectFileOpt optNone

buildExecutableOpt :: Optimizer -> FilePath -> AST.Module -> IO ()
buildExecutableOpt opt path ast = runJIT $ do
  tmpdir <- using $ managed $ withSystemTempDirectory "simply-llvm-"
  let objfile = tmpdir </> "simply.o"
  mainfile <- liftIO $ getDataFileName "data/main.c"
  liftIO $ writeObjectFileOpt opt (tmpdir </> "simply.o") ast
  liftIO $ callProcess "clang" [objfile, mainfile, "-o", path]

buildExecutable :: FilePath -> AST.Module -> IO ()
buildExecutable = buildExecutableOpt optNone


----------------------------------------------------------------------
-- Optimizers

type Optimizer = Module -> JIT ()

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

passManager :: PassSetSpec -> JIT PassManager
passManager passes =
  using $ managed (withPassManager passes)


----------------------------------------------------------------------
-- Error Handling

data JITError
  = MissingEntryPoint
  deriving (Show, Eq, Ord, Typeable)

instance Exception JITError


----------------------------------------------------------------------
-- The JIT

newtype JIT a = JIT (ReaderT Context Managed a)
  deriving
    ( Functor, Applicative, Monad
    , MonadReader Context, MonadIO, MonadManaged
    )

jitManaged :: JIT a -> Managed a
jitManaged (JIT m) = do
  ctx <- using $ managed withContext
  runReaderT m ctx

runJIT :: JIT () -> IO ()
runJIT = runManaged . jitManaged

withJIT :: JIT a -> (a -> IO r) -> IO r
withJIT = with . jitManaged

moduleFromAST :: AST.Module -> JIT Module
moduleFromAST ast = do
  ctx <- ask
  using $ managed (withModuleFromAST ctx ast)

llvmIRFromModule :: Module -> JIT ByteString
llvmIRFromModule = liftIO . moduleLLVMAssembly

assemblyFromModule :: Module -> JIT ByteString
assemblyFromModule m = do
  machine <- using $ managed withHostTargetMachine
  liftIO $ moduleTargetAssembly machine m

compile :: Module -> JIT ([Int32] -> IO Int32)
compile m = do
  let
    optlevel = Just 0
    model = Nothing
    framePtrElim = Nothing
    fastInstr = Nothing
  ctx <- ask
  engine <- using $ managed (withMCJIT ctx optlevel model framePtrElim fastInstr)
  bin <- using $ managed (withModuleInEngine engine m)
  mbMainFun <- liftIO $ getFunction bin (AST.Name $ mangle "main")
  mainFun <- maybe (throwIO MissingEntryPoint) pure mbMainFun
  pure (callFFI mainFun retInt32 . map argInt32)
