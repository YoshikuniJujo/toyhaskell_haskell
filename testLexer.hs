module Main where

import Prelude hiding ( lex )
import Lexer
import Eval
import Value
import Interact

main :: IO ()
main = runLoop "testLexer" $ \input -> do
	either print ( either print showValue . eval initEnv ) $
		toyParse input
