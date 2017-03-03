{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}

module Simply.Orphans
  (
  ) where

import Protolude hiding (Type, (<>))
import qualified Data.Map as Map
import Data.List.NonEmpty (NonEmpty)
import LLVM.AST
import LLVM.AST.AddrSpace
import LLVM.AST.CallingConvention
import LLVM.AST.COMDAT
import LLVM.AST.Constant
import LLVM.AST.DataLayout
import LLVM.AST.DLL
import LLVM.AST.Float
import LLVM.AST.FloatingPointPredicate
import LLVM.AST.FunctionAttribute
import LLVM.AST.InlineAssembly
import LLVM.AST.IntegerPredicate
import LLVM.AST.Linkage
import LLVM.AST.ParameterAttribute
import LLVM.AST.RMWOperation
import LLVM.AST.ThreadLocalStorage
import LLVM.AST.Visibility
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty (Out, docPrec, doc)


deriving instance Generic Module
instance Out Module

deriving instance Generic DataLayout
instance Out DataLayout

deriving instance Generic Definition
instance Out Definition

deriving instance Generic SelectionKind
instance Out SelectionKind

deriving instance Generic GroupID
instance Out GroupID

deriving instance Generic FunctionAttribute
instance Out FunctionAttribute

deriving instance Generic Type
instance Out Type

deriving instance Generic Operand
instance Out Operand

deriving instance Generic Metadata
instance Out Metadata

deriving instance Generic MetadataNode
instance Out MetadataNode

deriving instance Generic MetadataNodeID
instance Out MetadataNodeID

deriving instance Generic Global
instance Out Global

deriving instance Generic FloatingPointFormat
instance Out FloatingPointFormat

deriving instance Generic Name
instance Out Name

deriving instance Generic Parameter
instance Out Parameter

deriving instance Generic BasicBlock
instance Out BasicBlock

deriving instance Generic AddrSpace
instance Out AddrSpace

deriving instance Generic CallingConvention
instance Out CallingConvention

deriving instance Generic Visibility
instance Out Visibility

deriving instance Generic Linkage
instance Out Linkage

deriving instance Generic StorageClass
instance Out StorageClass

deriving instance Generic Model
instance Out Model

deriving instance Generic Constant
instance Out Constant

deriving instance Generic ParameterAttribute
instance Out ParameterAttribute

deriving instance Generic FloatingPointPredicate
instance Out FloatingPointPredicate

deriving instance Generic IntegerPredicate
instance Out IntegerPredicate

deriving instance Generic SomeFloat
instance Out SomeFloat

deriving instance Generic Terminator
instance Out Terminator

deriving instance Generic (Named a)
instance Out a => Out (Named a)

deriving instance Generic Instruction
instance Out Instruction

deriving instance Generic TailCallKind
instance Out TailCallKind

deriving instance Generic SynchronizationScope
instance Out SynchronizationScope

deriving instance Generic MemoryOrdering
instance Out MemoryOrdering

deriving instance Generic LandingPadClause
instance Out LandingPadClause

deriving instance Generic FastMathFlags
instance Out FastMathFlags

deriving instance Generic RMWOperation
instance Out RMWOperation

deriving instance Generic InlineAssembly
instance Out InlineAssembly

deriving instance Generic Dialect
instance Out Dialect

deriving instance Generic Mangling
instance Out Mangling

deriving instance Generic Endianness
instance Out Endianness

deriving instance Generic AlignmentInfo
instance Out AlignmentInfo

deriving instance Generic AlignType
instance Out AlignType

deriving instance Generic UnnamedAddr
instance Out UnnamedAddr


instance (Out a, Ord a) => Out (Set a) where
    docPrec _ = brackets . fsep . punctuate comma . map doc . toList
    doc = docPrec 0


instance (Out a, Out b, Ord a) => Out (Map a b) where
    docPrec _ = braces . fsep . punctuate comma . map f . Map.toList
      where
        f (key, val) = doc key <> colon <+> doc val
    doc = docPrec 0


instance (Out a) => Out (NonEmpty a)


instance Out Text where
    docPrec _ = doubleQuotes . text . toS
    doc = doubleQuotes . text . toS

instance Out Word where
    docPrec _ = text . show
    doc = text . show

instance Out Word16 where
    docPrec _ = text . show
    doc = text . show

instance Out Word32 where
    docPrec _ = text . show
    doc = text . show

instance Out Word64 where
    docPrec _ = text . show
    doc = text . show
