module Main where

import Lexer
import Preprocessor
import Prelude hiding ( lex )

main :: IO ()
main = interact $ ( ++ "\n" ) . unwords . map ( showLex . fst ) . prep 0 [] . lex ( initPos "" )

showLex :: Token -> String
showLex ( Variable x ) = x
showLex ( ReservedOp x ) = x
showLex ( Reserved x ) = x
showLex ( Operator x ) = x
showLex ( TokString x ) = show x
showLex ( TokInteger x ) = show x
showLex OpenParen = "("
showLex CloseParen = ")"
showLex OpenBrace = "{"
showLex CloseBrace = "}"
showLex x = show x
