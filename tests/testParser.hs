module Main where

import Interact
import Parser
import Lexer
import Preprocessor
import Prelude hiding ( lex )

main :: IO ()
main = runLoop "testParser" () $ \() input -> do
	opLst <- fmap getOpTable $ readFile "data/operator-table.lst"
	print $ toyParse opLst "testParser" $ prep 0 [] $ lex ( initialPos "" ) input
