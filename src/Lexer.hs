module Lexer (
	Token( .. ),
	lex
) where

import Prelude hiding ( lex )
import Data.Char

data Token =
	OpenBrace | CloseBrace |
	Variable String | TokConst String | Operator String | OpenParen |
	CloseParen | Backslash | Reserved String | ReservedOp String |
	TokInteger Integer | TokChar Char | TokString String
	deriving ( Show, Eq )

reserved, reservedOp :: [ String ]
reserved = [ "let", "in", "if", "then", "else", "case", "of" ]
reservedOp = [ "=", ";", "->", "[]", "[", "]", "," ]

lex :: String -> [ Token ]
lex "" = [ ]
lex ( '-' : '-' : cs )		= lex $ dropWhile ( /= '\n' ) cs
lex ( '{' : cs )		= OpenBrace : lex cs
lex ( '}' : cs )		= CloseBrace : lex cs
lex ( '[' : cs )		= ReservedOp "[" : lex cs
lex ( ']' : cs )		= ReservedOp "]" : lex cs
lex ( '(' : cs )		= OpenParen : lex cs
lex ( ')' : cs )		= CloseParen : lex cs
lex ( '\\' : cs )		= Backslash : lex cs
lex ( '\'' : '\\' : 'n' : '\'' : cs ) = TokChar '\n' : lex cs
lex ( '\'' : c : '\'' : cs )	= TokChar c : lex cs
lex ( '"' : cs )		= let ( ret, '"' : rest ) = span (/= '"') cs in
		TokString ret : lex rest
lex s@( c : cs )
	| isSpace c		= lex cs
	| isLow c		= let ( ret, rest ) = span isAlNum s in
		( if ret `elem` reserved then Reserved else Variable ) ret :
			lex rest
	| isUpper c		= let
		( ret, rest ) = span isAlphaNum s
		tok = case ret of
			_	-> TokConst ret in
		tok : lex rest
	| isSym c	= let ( ret, rest ) = span isSym s in
		( if ret `elem` reservedOp then ReservedOp else Operator ) ret :
			lex rest
	| isDigit c	= let ( ret, rest ) = span isDigit s in
		TokInteger ( read ret ) : lex rest
	where
	isSym cc = isSymbol cc || cc `elem` "\\-*;:,"
	isLow cc = isLower cc || cc `elem` "_"
	isAlNum cc = isAlphaNum cc || cc `elem` "_"
lex s			= error $ "lex failed: " ++ s
