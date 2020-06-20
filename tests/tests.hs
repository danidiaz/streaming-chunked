{-# LANGUAGE ImportQualifiedPost #-}
module Main where

import Test.Tasty

import Streaming
import Streaming.Prelude qualified as S
import Streaming.Byte qualified as SB
import Streaming.Byte.Chunk qualified as SBC
import Streaming.Char qualified as SC
import Streaming.Char.Chunk qualified as SCC

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = 
    testGroup "All" []

