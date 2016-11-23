module Simply.LLVM.Codegen where

import Protolude hiding (local, void, local, one, zero)
import Data.String (String, IsString(..))
import Data.Word
import Data.Function
import qualified Data.Map as Map
import qualified Data.List as List

import Control.Applicative

import LLVM.General.AST hiding (callingConvention, functionAttributes)
import LLVM.General.AST.Type
import LLVM.General.AST.Global
import qualified LLVM.General.AST as AST

import qualified LLVM.General.AST.Linkage as L
import qualified LLVM.General.AST.Constant as C
import qualified LLVM.General.AST.Attribute as A
import qualified LLVM.General.AST.IntegerPredicate as IP
import qualified LLVM.General.AST.FloatingPointPredicate as FP
import qualified LLVM.General.AST.CallingConvention as CC

-------------------------------------------------------------------------------
-- Module Level
-------------------------------------------------------------------------------

newtype LLVM a = LLVM (State AST.Module a)
  deriving (Functor, Applicative, Monad, MonadState AST.Module )

runLLVM :: AST.Module -> LLVM a -> AST.Module
runLLVM mod (LLVM m) = execState m mod

emptyModule :: String -> AST.Module
emptyModule label = defaultModule { moduleName = label }

addDefn :: Definition -> LLVM ()
addDefn d = do
  defs <- gets moduleDefinitions
  modify $ \s -> s { moduleDefinitions = defs ++ [d] }


-- | Define an externally visible function (used for main)
define ::  Type -> String -> [(Type, Name)] -> Codegen a -> LLVM ()
define retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = bls
    -- Needs to match calling convention in call
  , callingConvention = CC.Fast
  }
  where
    bls = createBlocks $ execCodegen $ do
      setBlock =<< addBlock entryBlockName
      body


-- | Define an internal function (can be optimised away)
internal ::  Type -> String -> [(Type, Name)] -> Codegen a -> LLVM ()
internal retty label argtys body = addDefn $
  GlobalDefinition $ functionDefaults {
    name        = Name label
  , linkage     = L.Private
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = bls
    -- Needs to match calling convention in call
  , callingConvention = CC.Fast
  }
  where
    bls = createBlocks $ execCodegen $ do
      setBlock =<< addBlock entryBlockName
      body


-- | Declare an external function that is not defined in the module
-- (used for malloc)
external ::  Type -> String -> [(Type, Name)] -> LLVM ()
external retty label argtys = addDefn $
  GlobalDefinition $ functionDefaults 
  { name        = Name label
  , linkage     = L.External
  , parameters  = ([Parameter ty nm [] | (ty, nm) <- argtys], False)
  , returnType  = retty
  , basicBlocks = []
  }


---------------------------------------------------------------------------------
-- Types
-------------------------------------------------------------------------------

int :: Type
int = IntegerType 32

ibool :: Type
ibool = IntegerType 1

-- | type of a function
fn :: Type -> [Type] -> Type
fn retty argty = FunctionType retty argty False

-- | function pointer
fnPtr :: Type -> [Type] -> Type
fnPtr retty argty = ptr $ fn retty argty

-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

cint = cons . C.Int 32
cint32 = cons . C.Int 32

one = cons $ C.Int 32 1
zero = cons $ C.Int 32 0

false = cons $ C.Int 1 0
true = cons $ C.Int 1 1

-------------------------------------------------------------------------------
-- Names
-------------------------------------------------------------------------------

type Names = Map.Map String Int

uniqueName :: String -> Names -> (String, Names)
uniqueName nm ns =
  case Map.lookup nm ns of
    Nothing -> (nm,  Map.insert nm 0 ns)
    Just ix -> (nm ++ show ix, Map.insert nm (ix+1) ns)

instance IsString Name where
  fromString = Name . fromString

-------------------------------------------------------------------------------
-- Codegen State
-------------------------------------------------------------------------------

type SymbolTable = [(String, Operand)]

data CodegenState
  = CodegenState 
  { currentBlock :: Name                     -- Name of the active block to append to
  , blocks       :: Map.Map Name BlockState  -- Blocks for function
  , symtab       :: SymbolTable              -- Function scope symbol table
  , blockCount   :: Int                      -- Count of basic blocks
  , count        :: Word                     -- Count of unnamed instructions
  , names        :: Names                    -- Name Supply
  } deriving Show

data BlockState
  = BlockState 
  { idx   :: Int                            -- Block index
  , stack :: [Named Instruction]            -- Stack of instructions
  , term  :: Maybe (Named Terminator)       -- Block terminator
  } deriving Show

-------------------------------------------------------------------------------
-- Codegen Operations
-------------------------------------------------------------------------------

newtype Codegen a = Codegen { runCodegen :: State CodegenState a }
  deriving (Functor, Applicative, Monad, MonadState CodegenState )

sortBlocks :: [(Name, BlockState)] -> [(Name, BlockState)]
sortBlocks = sortBy (compare `on` (idx . snd))

createBlocks :: CodegenState -> [BasicBlock]
createBlocks m = map makeBlock $ sortBlocks $ Map.toList (blocks m)

makeBlock :: (Name, BlockState) -> BasicBlock
makeBlock (l, BlockState _ s t) = BasicBlock l s (maketerm t)
  where
    maketerm (Just x) = x
    maketerm Nothing = panic $ "Block has no terminator: " <> show l

entryBlockName :: String
entryBlockName = "entry"

emptyBlock :: Int -> BlockState
emptyBlock i = BlockState i [] Nothing

emptyCodegen :: CodegenState
emptyCodegen = CodegenState (Name entryBlockName) Map.empty [] 1 0 Map.empty

execCodegen :: Codegen a -> CodegenState
execCodegen m = execState (runCodegen m) emptyCodegen

fresh :: Codegen Word
fresh = do
  i <- gets count
  modify $ \s -> s { count = 1 + i }
  return i

freshName :: Codegen AST.Name
freshName = AST.UnName <$> fresh

instr :: Type -> Instruction -> Codegen Operand
instr ty ins = do
  n <- fresh
  let ref = UnName n
  blk <- current
  let i = stack blk
  modifyBlock (blk { stack = i ++ [ref := ins] } )
  return $ local ty ref

terminator :: Named Terminator -> Codegen (Named Terminator)
terminator trm = do
  blk <- current
  modifyBlock (blk { term = Just trm })
  return trm

-------------------------------------------------------------------------------
-- Block Stack
-------------------------------------------------------------------------------

entry :: Codegen Name
entry = gets currentBlock

addBlock :: String -> Codegen Name
addBlock bname = do
  bls <- gets blocks
  ix <- gets blockCount
  nms <- gets names
  let new = emptyBlock ix
      (qname, supply) = uniqueName bname nms
  modify $ \s -> s { blocks = Map.insert (Name qname) new bls
                   , blockCount = ix + 1
                   , names = supply
                   }
  return (Name qname)

setBlock :: Name -> Codegen Name
setBlock bname = do
  modify $ \s -> s { currentBlock = bname }
  return bname

getBlock :: Codegen Name
getBlock = gets currentBlock

modifyBlock :: BlockState -> Codegen ()
modifyBlock new = do
  active <- gets currentBlock
  modify $ \s -> s { blocks = Map.insert active new (blocks s) }

current :: Codegen BlockState
current = do
  c <- gets currentBlock
  blks <- gets blocks
  case Map.lookup c blks of
    Just x -> return x
    Nothing -> panic $ "No such block: " <> show c

-------------------------------------------------------------------------------
-- Symbol Table
-------------------------------------------------------------------------------

assign :: String -> Operand -> Codegen ()
assign var x = do
  lcls <- gets symtab
  modify $ \s -> s { symtab = (var, x) : lcls }

getvar :: String -> Codegen Operand
getvar var = do
  syms <- gets symtab
  case List.lookup var syms of
    Just x  -> return x
    Nothing -> panic $ "Local variable not in scope: " <> show var

-------------------------------------------------------------------------------

-- References
local ::  Type -> Name -> Operand
local = LocalReference

global :: Type -> Name -> C.Constant
global = C.GlobalReference

-- | Refer to global function
externf :: Type -> Name -> Operand
externf ty nm = ConstantOperand (C.GlobalReference ty nm)

-- Arithmetic and Constants
fadd :: Operand -> Operand -> Codegen Operand
fadd a b = instr float $ FAdd NoFastMathFlags a b []

fsub :: Operand -> Operand -> Codegen Operand
fsub a b = instr float $ FSub NoFastMathFlags a b []

fmul :: Operand -> Operand -> Codegen Operand
fmul a b = instr float $ FMul NoFastMathFlags a b []

fdiv :: Operand -> Operand -> Codegen Operand
fdiv a b = instr float $ FDiv NoFastMathFlags a b []

fcmp :: FP.FloatingPointPredicate -> Operand -> Operand -> Codegen Operand
fcmp cond a b = instr float $ FCmp cond a b []

icmp :: IP.IntegerPredicate -> Operand -> Operand -> Codegen Operand
icmp cond a b = instr ibool $ ICmp cond a b []

trunc :: Type -> Operand -> Codegen Operand
trunc ty a = instr ty $ Trunc a ty []

cons :: C.Constant -> Operand
cons = ConstantOperand

nowrap :: Bool
nowrap = False

add :: Operand -> Operand -> Codegen Operand
add a b = instr int $ Add nowrap nowrap a b []

mul :: Operand -> Operand -> Codegen Operand
mul a b = instr int $ Mul nowrap nowrap a b []

sub :: Operand -> Operand -> Codegen Operand
sub a b = instr int $ Sub nowrap nowrap a b []

uitofp :: Type -> Operand -> Codegen Operand
uitofp ty a = instr float $ UIToFP a ty []

toArgs :: [Operand] -> [(Operand, [A.ParameterAttribute])]
toArgs = map (\x -> (x, []))

bitcast :: Type -> Operand -> Codegen Operand
bitcast ty a = instr ty $ BitCast a ty []

ptrtoint :: Type -> Operand -> Codegen Operand
ptrtoint ty a = instr ty $ PtrToInt a ty []

-- Effects

-- | call according to C calling convention
ccall :: Type -> Operand -> [Operand] -> Codegen Operand
ccall ty fn args = instr ty $ Call Nothing CC.C [] (Right fn) (toArgs args) [] []

-- | call according to fast calling convention
--
-- needs to match calling convention in function definition
call :: Type -> Operand -> [Operand] -> Codegen Operand
call ty fn args = instr ty $ Call Nothing CC.Fast [] (Right fn) (toArgs args) [] []

-- | allocate space on the stack
alloca :: Type -> Codegen Operand
alloca ty = instr ty $ Alloca ty Nothing 0 []

store :: Operand -> Operand -> Codegen Operand
store dst val = instr void $ Store False dst val Nothing 0 []

load :: Type -> Operand -> Codegen Operand
load ty ptr = instr ty $ Load False ptr Nothing 0 []

extractValue :: Type -> Operand -> [Word32] -> Codegen Operand
extractValue ty struct idx = instr ty $ ExtractValue struct idx []

insertValue :: Type -> Operand -> Operand -> [Word32] -> Codegen Operand
insertValue ty struct val idx = instr ty $ InsertValue struct val idx []

-- Control Flow
br :: Name -> Codegen (Named Terminator)
br val = terminator $ Do $ Br val []

cbr :: Operand -> Name -> Name -> Codegen (Named Terminator)
cbr cond tr fl = terminator $ Do $ CondBr cond tr fl []

phi :: Type -> [(Operand, Name)] -> Codegen Operand
phi ty incoming = instr int $ Phi ty incoming []

ret :: Operand -> Codegen (Named Terminator)
ret val = terminator $ Do $ Ret (Just val) []

retvoid :: Codegen (Named Terminator)
retvoid = terminator $ Do $ Ret Nothing []
