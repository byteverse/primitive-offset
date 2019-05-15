{-# language DuplicateRecordFields #-}
module Data.Primitive.ByteArray.Offset
  ( -- * Types
    ByteArrayOffset(..)
  , MutableByteArrayOffset(..)
  ) where

import Control.Monad.Primitive (PrimMonad,PrimState)
import Data.Primitive (ByteArray,MutableByteArray)

-- | A byte array and an index into the array.
data ByteArrayOffset = ByteArrayOffset
  { array :: {-# UNPACK #-} !ByteArray
  , index :: {-# UNPACK #-} !Int
  }

-- | A mutable byte array and an index into the array.
data MutableByteArrayOffset s = MutableByteArrayOffset
  { array :: {-# UNPACK #-} !(MutableByteArray s)
  , index :: {-# UNPACK #-} !Int
  }
