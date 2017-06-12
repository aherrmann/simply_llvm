{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}

module Simply.Orphans
  (
  ) where

import Protolude hiding (Type, (<>))

import qualified Data.Map as Map
import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as ShortByteString
import Data.List.NonEmpty (NonEmpty)
import Text.PrettyPrint
import Text.PrettyPrint.GenericPretty (Out, docPrec, doc)

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


instance Out AddrSpace
instance Out AlignmentInfo
instance Out AlignType
instance Out a => Out (Named a)
instance Out BasicBlock
instance Out CallingConvention
instance Out Constant
instance Out DataLayout
instance Out Definition
instance Out Dialect
instance Out Endianness
instance Out FastMathFlags
instance Out FloatingPointPredicate
instance Out FloatingPointType
instance Out FunctionAttribute
instance Out Global
instance Out GroupID
instance Out InlineAssembly
instance Out Instruction
instance Out IntegerPredicate
instance Out LandingPadClause
instance Out Linkage
instance Out Mangling
instance Out MemoryOrdering
instance Out Metadata
instance Out MetadataNode
instance Out MetadataNodeID
instance Out Model
instance Out Module
instance Out Name
instance Out Operand
instance Out Parameter
instance Out ParameterAttribute
instance Out RMWOperation
instance Out SelectionKind
instance Out SomeFloat
instance Out StorageClass
instance Out SynchronizationScope
instance Out TailCallKind
instance Out Terminator
instance Out Type
instance Out UnnamedAddr
instance Out Visibility

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

instance Out ByteString where
    docPrec _ = doubleQuotes . text . toS
    doc = doubleQuotes . text . toS

instance Out ShortByteString where
    docPrec _ = doubleQuotes . text . toS . ShortByteString.fromShort
    doc = doubleQuotes . text . toS . ShortByteString.fromShort

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


instance StringConv Text ShortByteString where
    strConv l = ShortByteString.toShort . strConv l

instance StringConv ShortByteString Text where
    strConv l = strConv l . ShortByteString.fromShort
