{-# LANGUAGE ImportQualifiedPost #-}
module Streaming.Char.Chunk where

import Prelude hiding (length)

import Data.Text
import Data.Text.Lazy.Builder qualified 
import Data.Word

type Chunk = Text

length :: Chunk -> Int
length = Data.Text.length

type Item = Char

type Builder = Data.Text.Lazy.Builder.Builder
