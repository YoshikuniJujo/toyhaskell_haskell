module Lexer (
	Token( .. ),
	lex
) where

import Prelude hiding ( lex )
import Text.ParserCombinators.Parsec.Pos
import Data.Char

data Token =
	OpenBrace | CloseBrace |
	Variable String | TokConst String | Operator String | OpenParen |
	CloseParen | Backslash | Reserved String | ReservedOp String |
	TokInteger Integer | TokChar Char | TokString String | NewLine
	deriving ( Show, Eq )

reserved, reservedOp :: [ String ]
reserved = [ "let", "in", "if", "then", "else", "case", "of" ]
reservedOp = [ "=", ";", "->", "[]", "[", "]", "," ]

next, nextLine :: SourcePos -> SourcePos
next = flip incSourceColumn 1
nextLine = flip incSourceLine 1 . flip setSourceColumn 0

isc :: SourcePos -> Int -> SourcePos
isc = incSourceColumn

lex :: SourcePos -> String -> [ ( Token, SourcePos ) ]
lex _ ""			= [ ]
lex sp ( '-' : '-' : cs )	= lex sp $ dropWhile ( /= '\n' ) cs
lex sp ( '\n' : cs )		= ( NewLine, sp ) : lex ( nextLine sp ) cs
lex sp ( ';' : cs )		= ( ReservedOp ";", sp ) : lex ( next sp ) cs
lex sp ( '{' : cs )		= ( OpenBrace, sp ) : lex ( next sp ) cs
lex sp ( '}' : cs )		= ( CloseBrace, sp ) : lex ( next sp ) cs
lex sp ( '[' : cs )		= ( ReservedOp "[", sp ) : lex ( next sp ) cs
lex sp ( ']' : cs )		= ( ReservedOp "]", sp ) : lex ( next sp ) cs
lex sp ( '(' : cs )		= ( OpenParen, sp ) : lex ( next sp ) cs
lex sp ( ')' : cs )		= ( CloseParen, sp ) : lex ( next sp ) cs
lex sp ( '\\' : cs )		= ( Backslash, sp ) : lex ( next sp ) cs
lex sp ( '\'' : '\\' : 'n' : '\'' : cs ) = ( TokChar '\n', sp ) : lex ( isc sp 2 ) cs
lex sp ( '\'' : c : '\'' : cs )	= ( TokChar c, sp ) : lex ( isc sp 3 ) cs
lex sp ( '"' : cs )		= let ( ret, '"' : rest ) = span (/= '"') cs in
		( TokString ret, sp ) : lex ( isc sp $ length ret + 2 ) rest
lex sp ( '`' : cs )		= let ( ret, '`' : rest ) = span ( /= '`' ) cs in
		( Operator ret, sp ) : lex ( isc sp $ length ret + 2 ) rest
lex sp ( '\t' : cs )		= let c = sourceColumn sp in
					lex ( setSourceColumn  sp ( 8 * ( c `div` 8 + 1 ) + 1 ) ) cs
lex sp s@( c : cs )
	| isSpace c		= lex ( next sp ) cs
	| isLow c		= let ( ret, rest ) = span isAlNum s in
		( ( if ret `elem` reserved then Reserved else Variable ) ret, sp ) :
			lex ( isc sp $ length ret ) rest
	| isUpper c		= let
		( ret, rest ) = span isAlphaNum s
		tok = case ret of
			_	-> TokConst ret in
		( tok, sp ) : lex ( isc sp $ length ret ) rest
	| isSym c	= let ( ret, rest ) = span isSym s in
		( ( if ret `elem` reservedOp then ReservedOp else Operator ) ret, sp ) :
			lex ( isc sp $ length ret ) rest
	| isDigit c	= let ( ret, rest ) = span isDigit s in
		( TokInteger ( read ret ), isc sp $ length ret ) : lex sp rest
	where
	isSym cc = isSymbol cc || cc `elem` "\\-*;:,/"
	isLow cc = isLower cc || cc `elem` "_"
	isAlNum cc = isAlphaNum cc || cc `elem` "_"
lex sp s			= error $ "lex failed: " ++ show sp ++ s
