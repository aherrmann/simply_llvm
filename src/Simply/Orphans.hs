{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Simply.Orphans () where

import Protolude hiding (Type, (<>))

import Data.ByteString.Short (ShortByteString)
import qualified Data.ByteString.Short as ShortByteString


instance StringConv ShortByteString Text where
  strConv l = strConv l . ShortByteString.fromShort

instance StringConv Text ShortByteString where
  strConv l = ShortByteString.toShort . strConv l
