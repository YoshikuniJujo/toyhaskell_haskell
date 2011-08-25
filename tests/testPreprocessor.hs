module Main where

import Lexer
import Preprocessor

main :: IO ()
main = interact $ ( ++ "\n" ) . unwords . map ( showLex . fst ) . prep 0 [] . toyLex ""

showLex :: Token -> String
showLex ( Varid x ) = x
showLex ( ReservedOp x ) = x
showLex ( ReservedId x ) = x
showLex ( VarSym x ) = x
showLex ( TokString x ) = show x
showLex ( TokInteger x ) = show x
showLex ( Special '(' ) = "("
showLex ( Special ')' ) = ")"
-- showLex OpenBrace = "{"
showLex ( Special '{' ) = "{"
showLex ( Special '}' ) = "}"
showLex x = show x
