{-# Language PackageImports #-}

module Lexer (
	toyLex,
	toyLex',
	lexer,
	SourceName,
) where

import Prelude hiding ( lex )
import Types ( Token( .. ), ParserMonad )
import Text.ParserCombinators.Parsec.Pos ( SourcePos, SourceName, initialPos,
	sourceColumn, setSourceColumn, incSourceColumn, incSourceLine )
import Data.Char ( isLower, isUpper, isAlphaNum, isDigit )
import "monads-tf" Control.Monad.State

reserved, reservedOp :: [ String ]
reserved = [
	"case", "class", "data", "default", "deriving", "do", "else", "if",
	"import", "in", "infix", "infixl", "infixr", "instance", "let",
	"module", "newtype", "of", "then", "type", "where", "_"
 ]
reservedOp = [ "..", {-":",-} "::", "=", "\\", "|", "<-", "->", "@", "~", "=>" ]

special :: [ Char ]
special = "(),;[]{}" -- ++ [ '`' ]

next, nextLine :: SourcePos -> SourcePos
next		= flip incSourceColumn 1
nextLine	= flip incSourceLine 1 . flip setSourceColumn 0

isc :: SourcePos -> Int -> SourcePos
isc = incSourceColumn

toyLex :: SourceName -> String -> [ ( Token, SourcePos ) ]
toyLex = lex . initialPos

toyLex' :: SourceName -> String -> [ Token ]
toyLex' sn = map fst . toyLex sn

lexer :: ( Token -> ParserMonad a ) -> ParserMonad a
lexer cont = real_lexer >>= \token -> cont token

real_lexer :: ParserMonad Token
real_lexer = do
	( ( tok, _ ), _, rest ) <- fmap ( spanLex $ initialPos "" ) get
	if tok == ReservedId "let" then do
		case spanLex ( initialPos "" ) rest of
			( ( Special '{', _ ), _, _ )	-> put rest
			_				-> put $ '{' : rest
		else put rest
	return tok

lex :: SourcePos -> String -> [ ( Token, SourcePos ) ]
lex _ ""			= [ ]
lex sp ( '-' : '-' : cs )	= lex sp $ dropWhile ( /= '\n' ) cs
lex sp ( '\n' : cs )		= ( NewLine, sp ) : lex ( nextLine sp ) cs
lex sp ( ' ' : cs )		= lex ( next sp ) cs
lex sp ( '\t' : cs )		= let c = sourceColumn sp in
	lex ( setSourceColumn  sp ( 8 * ( c `div` 8 + 1 ) + 1 ) ) cs
lex sp ca		= t : lex nsp rest
	where ( t, nsp, rest ) = spanLex sp ca

spanLex :: SourcePos -> String -> ( ( Token, SourcePos ), SourcePos, String )
spanLex sp ""			= ( ( TokenEOF, sp ), sp, "" )
spanLex sp ( '-' : '-' : cs )	= spanLex sp $ dropWhile ( /= '\n' ) cs
spanLex sp ( ' ' : cs )		= spanLex ( next sp ) cs
spanLex sp ( '\t' : cs )	= let c = sourceColumn sp in
	spanLex ( setSourceColumn  sp ( 8 * ( c `div` 8 + 1 ) + 1 ) ) cs
spanLex sp ( '\n' : cs )	= ( ( NewLine, sp ), nextLine sp, cs )
spanLex sp ( '\'' : '\\' : 'n' : '\'' : cs )
				= ( ( TokChar '\n', sp ), isc sp 4, cs )
spanLex sp ( '\'' : c : '\'' : cs )
				= ( ( TokChar c, sp ), isc sp 3, cs )
spanLex sp ( '"' : cs )		= let ( ret, '"' : rest ) = span (/= '"') cs in
	( ( TokString ret, sp ), isc sp $ length ret + 2, rest )
spanLex sp ( '`' : cs )		= let ( ret, '`' : rest ) = span ( /= '`' ) cs in
	( ( VarSym ret, sp ), isc sp $ length ret + 2, rest )
spanLex sp s@( c : cs )
	| c `elem` special	= ( ( Special c, sp ), next sp, cs )
	| isLow c		= let
		( ret, rest )	= span isAlNum s
		mkTok		= if ret `elem` reserved
					then ReservedId else Varid in
		( ( mkTok ret, sp ), isc sp $ length ret, rest )
	| isUpper c		= let
		( ret, rest )	= span isAlphaNum s in
		( ( Conid ret, sp ), isc sp $ length ret, rest )
	| isSym c		= let
		( ret, rest )	= span isSym s
		mkTok		= if ret `elem` reservedOp
					then ReservedOp else VarSym in
		( ( mkTok ret, sp ), isc sp $ length ret, rest )
	| isDigit c	= let ( ret, rest ) = span isDigit s in
		( ( TokInteger ( read ret ), isc sp $ length ret ), sp, rest )
	where
	isSym		= ( `elem` "!#$%&*+./<=>?@\\^|-~:" )
	isLow cc	= isLower cc || cc `elem` "_"
	isAlNum cc	= isAlphaNum cc || cc `elem` "_"
spanLex sp s			= error $ "spanLex failed: " ++ show sp ++ s
