module Main where

import Interact
import Lexer
import Preprocessor
import Prelude hiding ( lex )

main = runLoop "testLexer" () $ \() input ->
	print $ prep 0 [ ] $ lex ( initPos "testLexer" ) input
