{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Streaming.Chunked (
        Stream
    ) where

import Data.Kind
import Streaming qualified as S
import Streaming.Prelude qualified as S
import Streaming.Chunked.Chunk qualified as C
import Control.Monad.State (MonadState(..))
import Control.Monad.Reader (MonadReader(..))
import Control.Monad.Except (MonadError(..))
import Data.Coerce

type Stream :: (Type -> Type) -> Type -> Type
newtype Stream m r = Stream { getStream :: S.Stream (S.Of C.Chunk) m r }
    deriving newtype (
        Functor,
        Applicative,
        Monad,
        MonadFail,
        S.MonadIO,
        S.MonadTrans,
        S.MMonad,
        S.MFunctor)

instance (MonadReader r m) => MonadReader r (Stream m) where
  ask = coerce $ ask @_ @(S.Stream (S.Of C.Chunk) m)
  {-# INLINE ask #-}
  local = coerce $ local @_ @(S.Stream (S.Of C.Chunk) m)
  {-# INLINE local #-}

instance (MonadState s m) => MonadState s (Stream m) where
  get = coerce $ get @_ @(S.Stream (S.Of C.Chunk) m)
  {-# INLINE get #-}
  put = coerce $ put @_ @(S.Stream (S.Of C.Chunk) m)
  {-# INLINE put #-}
  state = coerce $ state @_ @(S.Stream (S.Of C.Chunk) m)
  {-# INLINE state #-}

instance (MonadError e m) => MonadError e (Stream m) where
  throwError = coerce $ throwError @_ @(S.Stream (S.Of C.Chunk) m)
  {-# INLINE throwError #-}
  catchError = coerce $ catchError @_ @(S.Stream (S.Of C.Chunk) m)
  {-# INLINABLE catchError #-}

