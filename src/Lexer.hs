{-# Language PackageImports #-}

module Lexer (
	toyLex,
	toyLex',
	lexer,
	SourceName,
	popIndents,
	prep'
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
lexer cont = prep' >>= cont

countWhites :: Int -> String -> Int
countWhites n ( ' ' : cs )	= countWhites ( n + 1 ) cs
countWhites n ( '\t' : cs )	= countWhites ( 8 * ( n `div` 8 + 1 ) + 1 ) cs
countWhites n _			= n

addOpenBrace :: ParserMonad ()
addOpenBrace = do
	( idnt1, idnta, pos, src, buf ) <- get
	put ( idnt1, idnta, pos, '{' : src, buf )

prep' :: ParserMonad Token
prep' = do
	t <- prep
	ma@( ~ ( m : ms ) ) <- getIndents
	case t of
		Indent n	-> do
			case ma of
				[ ] -> prep'
				( m : ms )
					| m == n	-> return $ Special ';'-- pushBackBuf ( Special ';', 0 )
					| n < m		-> do
--						error $ "debug " ++ show n ++ " " ++ show m
						putIndents ms
--						pushBackBuf ( Special '}', 0 )
						return $ Special '}'
					| otherwise	-> return () >> prep'
--			prep'
		AddBrace n	-> do
			putIndents $ n : ma
			return $ Special '{'
		Special '}'	-> case ma of
			[ ] -> error $ "bad ma"
			m : ms -> if m == 0 then popIndents >> return t else error "bad close brace"
		Special '{'	-> putIndents ( 0 : ma ) >> return t
		TokenEOF	-> if null ma then return t else do
			putIndents ms
			return $ Special '}'
		_		-> return t

popIndents :: ParserMonad Int
popIndents = do
	m : ms <- getIndents
	putIndents ms
	return m

putIndents :: [ Int ] -> ParserMonad ()
putIndents idnta = do
	( idnt1, _, pos, src, buf ) <- get
	put ( idnt1, idnta, pos, src, buf )

getIndents :: ParserMonad [ Int ]
getIndents = do
	( _idnt1, idnta, _pos, _src, _buf ) <- get
	return idnta

prep :: ParserMonad Token
prep = do
	( t, _ ) <- realLexer'
	case t of
		ReservedId "let" -> do
			nt <- realLexerNoNewLine
			case nt of
				( Special '{', _ )	-> pushBackBuf nt >> return ()
				( TokenEOF, _ )		->
					pushBackBuf nt >> pushBackBuf ( AddBrace 0, 0 )
				( _, cols )		->
					pushBackBuf nt >> pushBackBuf ( AddBrace cols, 0 )
			return t
		NewLine		-> do
			nt <- realLexerNoNewLine
			pushBackBuf nt
			let ( _, cols ) = nt
			pushBackBuf $ ( Indent cols, 0 )
			prep
		_		-> return t

peekToken :: ParserMonad Token
peekToken = do
	( _, _, _, src, _ ) <- get
	return $ one $ spanLex src
	where
	one ( x, _, _ ) = x

realLexerNoNewLine :: ParserMonad ( Token, Int )
realLexerNoNewLine = do
	t <- realLexer'
	case t of
		( NewLine, _ )	-> realLexerNoNewLine
		_		-> return t

realLexer' :: ParserMonad ( Token, Int )
realLexer' = do
	( idnt1, idnta, pos, src, buf@( ~( t : ts ) ) ) <- get
	if null buf then realLexer else do
		put ( idnt1, idnta, pos, src, ts )
		return t

pushBuf :: ( Token, Int ) -> ParserMonad ()
pushBuf t = do
	( idnt1, idnta, pos, src, buf ) <- get
	put ( idnt1, idnta, pos, src, buf ++ [ t ] )

pushBackBuf :: ( Token, Int ) -> ParserMonad ()
pushBackBuf t = do
	( idnt1, idnta, pos, src, buf ) <- get
	put ( idnt1, idnta, pos, src, t : buf )

popBuf :: ParserMonad ( Maybe ( Token, Int ) )
popBuf = do
	( idnt1, idnta, pos, src, buf@( ~( t : ts ) ) ) <- get
	if null buf then return Nothing else do
		put ( idnt1, idnta, pos, src, ts )
		return $ Just t

resetCols :: ParserMonad ()
resetCols = do
	( idnt1, idnta, ( lns, _ ), src, buf ) <- get
	put ( idnt1, idnta, ( lns, 1 ), src, buf )

realLexer :: ParserMonad ( Token, Int )
realLexer = do
	( idnt1, idnta{- @( ~( idnt0 : idnts ) ) -}, ( lns, cols ), src_, buf ) <- get
	let	( white, src ) = span ( `elem` " \t" ) src_
		ncols = countWhites cols white
	case spanLex src of
		( NewLine, _, rest )	-> do
			put ( idnt1, idnta, ( lns + 1, 1 ), rest, buf )
			return ( NewLine, ncols )
		( tok, c, rest )	-> do
			put ( idnt1, idnta, ( lns, ncols + c ), rest, buf )
			return ( tok, ncols )

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
