module Lexer (
	toyLex,
	SourceName
) where

import Prelude hiding ( lex )
import Types ( Token( .. ) )
import Text.ParserCombinators.Parsec.Pos ( SourcePos, SourceName, initialPos,
	sourceColumn, setSourceColumn, incSourceColumn, incSourceLine )
import Data.Char ( isLower, isUpper, isAlphaNum, isDigit )

reserved, reservedOp :: [ String ]
reserved = [
	"case", "class", "data", "default", "deriving", "do", "else", "if",
	"import", "in", "infix", "infixl", "infixr", "instance", "let",
	"module", "newtype", "of", "then", "type", "where", "_"
 ]
reservedOp = [ "..", {-":",-} "::", "=", "\\", "|", "<-", "->", "@", "~", "=>" ]

next, nextLine :: SourcePos -> SourcePos
next		= flip incSourceColumn 1
nextLine	= flip incSourceLine 1 . flip setSourceColumn 0

isc :: SourcePos -> Int -> SourcePos
isc = incSourceColumn

toyLex :: SourceName -> String -> [ ( Token, SourcePos ) ]
toyLex = lex . initialPos

lex :: SourcePos -> String -> [ ( Token, SourcePos ) ]
lex _ ""			= [ ]
lex sp ( '-' : '-' : cs )	= lex sp $ dropWhile ( /= '\n' ) cs
lex sp ( '\n' : cs )		= ( NewLine, sp ) : lex ( nextLine sp ) cs
lex sp ( ' ' : cs )		= lex ( next sp ) cs
lex sp ( ',' : cs )		= ( ReservedOp ",", sp ) : lex ( next sp ) cs
lex sp ( ';' : cs )		= ( ReservedOp ";", sp ) : lex ( next sp ) cs
lex sp ( '{' : cs )		= ( Special '{', sp ) : lex ( next sp ) cs
lex sp ( '}' : cs )		= ( Special '}', sp ) : lex ( next sp ) cs
lex sp ( '[' : cs )		= ( ReservedOp "[", sp ) : lex ( next sp ) cs
lex sp ( ']' : cs )		= ( ReservedOp "]", sp ) : lex ( next sp ) cs
lex sp ( '(' : cs )		= ( Special '(', sp ) : lex ( next sp ) cs
lex sp ( ')' : cs )		= ( Special ')', sp ) : lex ( next sp ) cs
lex sp ( '\\' : cs )		= ( ReservedOp "\\", sp ) : lex ( next sp ) cs
lex sp ( '\'' : '\\' : 'n' : '\'' : cs )
				= ( TokChar '\n', sp ) : lex ( isc sp 4 ) cs
lex sp ( '\'' : c : '\'' : cs )	= ( TokChar c, sp ) : lex ( isc sp 3 ) cs
lex sp ( '"' : cs )		= let ( ret, '"' : rest ) = span (/= '"') cs in
	( TokString ret, sp ) : lex ( isc sp $ length ret + 2 ) rest
lex sp ( '`' : cs )		= let ( ret, '`' : rest ) = span ( /= '`' ) cs in
	( VarSym ret, sp ) : lex ( isc sp $ length ret + 2 ) rest
lex sp ( '\t' : cs )		= let c = sourceColumn sp in
	lex ( setSourceColumn  sp ( 8 * ( c `div` 8 + 1 ) + 1 ) ) cs
lex sp s@( c : _ )
	| isLow c		= let
		( ret, rest )	= span isAlNum s
		mkTok		= if ret `elem` reserved
					then ReservedId else Varid in
		( mkTok ret, sp ) : lex ( isc sp $ length ret ) rest
	| isUpper c		= let
		( ret, rest )	= span isAlphaNum s in
		( Conid ret, sp ) : lex ( isc sp $ length ret ) rest
	| isSym c		= let
		( ret, rest )	= span isSym s
		mkTok		= if ret `elem` reservedOp
					then ReservedOp else VarSym in
		( mkTok ret, sp ) : lex ( isc sp $ length ret ) rest
	| isDigit c	= let ( ret, rest ) = span isDigit s in
		( TokInteger ( read ret ), isc sp $ length ret ) : lex sp rest
	where
	isSym		= ( `elem` "!#$%&*+./<=>?@\\^|-~:" )
	isLow cc	= isLower cc || cc `elem` "_"
	isAlNum cc	= isAlphaNum cc || cc `elem` "_"
lex sp s			= error $ "lex failed: " ++ show sp ++ s
