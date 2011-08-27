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

special :: String
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
lexer cont = realLexer >>= cont

countWhites :: Int -> String -> Int
countWhites n ( ' ' : cs )	= countWhites ( n + 1 ) cs
countWhites n ( '\t' : cs )	= countWhites ( 8 * ( n `div` 8 + 1 ) + 1 ) cs
countWhites n _			= n

realLexer :: ParserMonad Token
realLexer = do
	( idnt1, idnta@( ~( idnt0 : idnts ) ), ( lns, cols ), src_ ) <- get
	let	( white, src ) = span ( `elem` " \t" ) src_
		ncols = countWhites cols white
	case spanLex src of
		( tok@( Special '}' ), c, rest ) -> do
			when ( idnt0 /= 0 ) $ error "bad indent"
			put ( idnt1, idnts, ( lns, ncols + c ), rest )
			return tok
		( NewLine, _, rest ) -> do
			let idnt = countWhites 0 rest
			put ( idnt, idnta, ( lns + 1, 1 ), rest )
			realLexer
		( tok@( ReservedId "let" ), c, rest ) -> do
			case spanLex rest of
				( Special '{', c', _ )	->
					put ( idnt1, 0 : idnta, ( lns, ncols + c + c' ), rest )
				( _, c', _ )		->
					put ( idnt1, ncols + c + c' : idnta, ( lns, ncols + c ), '{' : rest )
			return tok
		( tok@( ReservedId "where" ), c, rest ) -> do
			case spanLex rest of
				( Special '{', c', _ )
					-> put ( idnt1, 0 : idnta, ( lns, ncols + c + c' ), rest )
				( _, c', _ )		->
					put ( idnt1, ncols + c + c' : idnta, ( lns, ncols + c ), '{' : rest )
--				_	-> put ( idnt1, idnta, ( lns, ncols + c ), '{' : rest )
			return tok
		( tok, c, rest ) -> do
			put ( idnt1, idnta, ( lns, ncols + c ), rest )
			return tok

spanLex :: String -> ( Token, Int, String )
spanLex ""			= ( TokenEOF, 0, "" )
spanLex ( '-' : '-' : cs )	= spanLex $ dropWhile ( /= '\n' ) cs
spanLex ( ' ' : cs )		= spanLex cs
spanLex ( '\t' : cs )		= spanLex cs
spanLex ( '\n' : cs )		= ( NewLine, 0, cs )
spanLex ( '\'' : '\\' : 'n' : '\'' : cs )
				= ( TokChar '\n', 4, cs )
spanLex ( '\'' : c : '\'' : cs )
				= ( TokChar c, 3, cs )
spanLex ( '"' : cs )		= let ( ret, '"' : rest ) = span (/= '"') cs in
	( TokString ret, length ret + 2, rest )
spanLex ( '`' : cs )		= let ( ret, '`' : rest ) = span ( /= '`' ) cs in
	( VarSym ret, length cs + 2, rest )
spanLex s@( c : cs )
	| c `elem` special	= ( Special c, 1, cs )
	| isLow c		= let
		( ret, rest )	= span isAlNum s
		mkTok		= if ret `elem` reserved
					then ReservedId else Varid in
		( mkTok ret, length ret, rest )
	| isUpper c		= let
		( ret, rest )	= span isAlphaNum s in
		( Conid ret, length ret, rest )
	| isSym c		= let
		( ret, rest )	= span isSym s
		mkTok		= if ret `elem` reservedOp
					then ReservedOp else VarSym in
		( mkTok ret, length ret, rest )
	| isDigit c	= let ( ret, rest ) = span isDigit s in
		( TokInteger ( read ret ), length ret, rest )
	where
	isSym		= ( `elem` "!#$%&*+./<=>?@\\^|-~:" )
	isLow cc	= isLower cc || cc `elem` "_"
	isAlNum cc	= isAlphaNum cc || cc `elem` "_"
spanLex s			= error $ "spanLex failed: " ++ s

lex :: SourcePos -> String -> [ ( Token, SourcePos ) ]
lex _ ""			= [ ]
lex sp ( '-' : '-' : cs )	= lex sp $ dropWhile ( /= '\n' ) cs
lex sp ( '\n' : cs )		= ( NewLine, sp ) : lex ( nextLine sp ) cs
lex sp ( ' ' : cs )		= lex ( next sp ) cs
lex sp ( '\t' : cs )		= let c = sourceColumn sp in
	lex ( setSourceColumn  sp ( 8 * ( c `div` 8 + 1 ) + 1 ) ) cs
lex sp ca		= t : lex nsp rest
	where ( t, nsp, rest ) = spanLex' sp ca

spanLex' :: SourcePos -> String -> ( ( Token, SourcePos ), SourcePos, String )
spanLex' sp ""			= ( ( TokenEOF, sp ), sp, "" )
spanLex' sp ( '-' : '-' : cs )	= spanLex' sp $ dropWhile ( /= '\n' ) cs
spanLex' sp ( ' ' : cs )		= spanLex' ( next sp ) cs
spanLex' sp ( '\t' : cs )	= let c = sourceColumn sp in
	spanLex' ( setSourceColumn  sp ( 8 * ( c `div` 8 + 1 ) + 1 ) ) cs
spanLex' sp ( '\n' : cs )	= ( ( NewLine, sp ), nextLine sp, cs )
-- spanLex sp ( '\n' : cs )	= spanLex' ( nextLine sp ) cs
spanLex' sp ( '\'' : '\\' : 'n' : '\'' : cs )
				= ( ( TokChar '\n', sp ), isc sp 4, cs )
spanLex' sp ( '\'' : c : '\'' : cs )
				= ( ( TokChar c, sp ), isc sp 3, cs )
spanLex' sp ( '"' : cs )		= let ( ret, '"' : rest ) = span (/= '"') cs in
	( ( TokString ret, sp ), isc sp $ length ret + 2, rest )
spanLex' sp ( '`' : cs )		= let ( ret, '`' : rest ) = span ( /= '`' ) cs in
	( ( VarSym ret, sp ), isc sp $ length ret + 2, rest )
spanLex' sp s@( c : cs )
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
spanLex' sp s			= error $ "spanLex' failed: " ++ show sp ++ s
