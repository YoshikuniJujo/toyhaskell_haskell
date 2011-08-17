module Main where

import Prelude hiding ( lex )
import Parser
import Eval
import Value
import Interact

main :: IO ()
main = runLoop "testLexer" $ \input -> do
	let ret = eval initEnv $ toyParse input
	showValue ret
