module Main where

import Interact
import Parser
import Value
import Eval
import Maybe

main :: IO ()
main = runLoop "toyhaskell" $ \input -> do
	either print ( either print showValue . eval initEnv ) $
		toyParse input
