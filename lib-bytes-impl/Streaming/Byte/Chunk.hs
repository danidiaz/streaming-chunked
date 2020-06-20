{-# LANGUAGE ImportQualifiedPost #-}
module Streaming.Byte.Chunk (
        Chunk,
        Item,
        Builder,
        module Data.ByteString
    ) where

import Prelude hiding (length)

import Data.ByteString
import Data.ByteString.Builder qualified 
import Data.Word

type Chunk = ByteString 

type Item = Word8

type Builder = Data.ByteString.Builder.Builder

