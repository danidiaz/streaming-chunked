{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneKindSignatures #-}
module Streaming.Chunked (
        Stream
    ) where

import Data.Kind
import Streaming qualified as S
import Streaming.Prelude qualified as S
import Streaming.Chunked.Chunk qualified as C

type Stream :: (Type -> Type) -> Type -> Type
data Stream m r = Stream { getStream :: S.Stream (S.Of C.Chunk) m r }
