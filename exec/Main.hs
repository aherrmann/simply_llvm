import Protolude

import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)

import Simply.Surface.Parse
import Simply.Surface.TypeCheck
import qualified Simply.Intermediate.FromSurface as Intermediate
import qualified Simply.LLVM.FromIntermediate as LLVM
import qualified Simply.LLVM.JIT as JIT


main :: IO ()
main = do
  args <- getArgs
  case args of
    [infile, outfile] ->
      compiler infile outfile
    _ -> usage >> exitFailure

compiler :: FilePath -> FilePath -> IO ()
compiler infile outfile = do
  ast <- parseFile' infile
  checked <- typeCheck' ast
  let
    intermediate = Intermediate.fromSurface checked
    llvmast = LLVM.fromIntermediate intermediate
  JIT.buildExecutableOpt JIT.optTarget outfile llvmast

usage :: IO ()
usage = do
  progname <- getProgName
  hPutStrLn stderr $
    "Usage: " <> progname <> " <infile> <outfile>\n\
    \\n\
    \Compiles the Simply program in <infile> to an executable in <outfile>.\n"
