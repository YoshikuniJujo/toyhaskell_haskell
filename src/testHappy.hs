{-# LANGUAGE PackageImports #-}

module Main where

import Interact
import NewParser
import Eval
import Primitives
import Types
import "monads-tf" Control.Monad.State

main :: IO ()
main = runLoop "testHappy" () $ \() input -> do
		showValue $ eval initEnv $ flip evalState input $ toyParse
