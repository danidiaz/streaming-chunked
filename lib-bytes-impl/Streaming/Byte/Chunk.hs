module Streaming.Byte.Chunk where

import Prelude hiding (length)

import Data.ByteString
import Data.ByteString.Builder
import Data.Word

type Chunk = ByteString 

length :: Chunk -> Int
length = Data.ByteString.length

type Item = Word8

type Builder = Data.ByteString.Builder.Builder

