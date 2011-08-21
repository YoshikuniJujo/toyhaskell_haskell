module Main where

import Interact
import Lexer
import Prelude hiding ( lex )

main = runLoop "testLexer" () $ \() input -> do
	print $ lex ( initPos "testLexer" ) input
