{-# Language PackageImports #-}

module Lexer (
	lexer,
	popIndents,
) where

import Prelude hiding ( lex )
import Types ( Token( .. ), ParserMonad )
import Data.Char ( isLower, isUpper, isAlphaNum, isDigit, isSpace )
import "monads-tf" Control.Monad.State

--------------------------------------------------------------------------------

reserved, reservedOp :: [ String ]
reserved = [
	"case", "class", "data", "default", "deriving", "do", "else", "if",
	"import", "in", "infix", "infixl", "infixr", "instance", "let",
	"module", "newtype", "of", "then", "type", "where", "_"
 ]
reservedOp = [ "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>" ]

special :: String
special = "(),;[]{}`"

lexer :: ( Token -> ParserMonad a ) -> ParserMonad a
lexer cont = prep' >>= cont

prep' :: ParserMonad Token
prep' = do
	t <- prep
	ma <- getIndents
	case t of
		Indent n	->
			case ma of
				[ ] -> prep'
				( m : ms )
					| m == n	-> return $ Special ';'
					| n < m		-> do
						putIndents ms
						pushBackBuf ( t, 0 )
						return $ Special '}'
					| otherwise	-> return () >> prep'
		AddBrace n	-> do
			putIndents $ n : ma
			return $ Special '{'
		Special '}'	-> case ma of
			[ ] -> error "bad ma"
			m : _ -> if m == 0 then popIndents >> return t else error "bad close brace"
		Special '{'	-> putIndents ( 0 : ma ) >> return t
		TokenEOF	-> if null ma then return t else do
			_ <- popIndents
			return $ Special '}'
		_		-> return t

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
		ReservedId "where" -> do
			nt <- realLexerNoNewLine
			case nt of
				( Special '{', _ )	-> pushBackBuf nt >> return ()
				( TokenEOF, _ )		->
					pushBackBuf nt >> pushBackBuf ( AddBrace 0, 0 )
				( _, cols )		->
					pushBackBuf nt >> pushBackBuf ( AddBrace cols, 0 )
			return t
		ReservedId "of" -> do
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
			pushBackBuf ( Indent cols, 0 )
			prep
		_		-> return t

realLexerNoNewLine :: ParserMonad ( Token, Int )
realLexerNoNewLine = do
	t <- realLexer'
	case t of
		( NewLine, _ )	-> realLexerNoNewLine
		_		-> return t

realLexer' :: ParserMonad ( Token, Int )
realLexer' = do
	t <- popBuf
	maybe realLexer_ return t

realLexer_ :: ParserMonad ( Token, Int )
realLexer_ = do
	( _, _, ( _, cols ), src, _ ) <- get
	let ( t, fin, rest ) = spanLex src
	updatePos fin
	putSrc rest
	return ( t, cols )

spanLex :: String -> ( Token, String, String )
spanLex src = let
	( t, fin, rest ) = getToken src
	( ws, rest' ) = readWhite rest in
	( t, fin ++ ws, rest' )

getToken :: String -> ( Token, String, String )
getToken = spanLex_

spanLex_ :: String -> ( Token, String, String )
spanLex_ ""			= ( TokenEOF, "", "" )
spanLex_ ( '\n' : cs )		= ( NewLine, "\n", cs )
spanLex_ ( '\'' : '\\' : 'n' : '\'' : cs )
				= ( TokChar '\n', "'\\n'", cs )
spanLex_ ( '\'' : c : '\'' : cs )
				= ( TokChar c, [ '\'', c, '\'' ], cs )
spanLex_ ( '"' : cs )		= let ( ret, '"' : rest ) = span (/= '"') cs in
	( TokString ret, '"' : ret ++ "\"", rest )
spanLex_ ( '`' : cs )		= let ( ret, '`' : rest ) = span ( /= '`' ) cs in
	( VarSym ret, '`' : ret ++ "`", rest )
spanLex_ ( '-' : '-' : _ )	= error "bad"
spanLex_ s@( c : cs )
	| isSpace c		= error "bad"
	| c `elem` special	= ( Special c, [ c ], cs )
	| isLow c		= let
		( ret, rest )	= span isAlNum s
		mkTok		= if ret `elem` reserved
					then ReservedId else Varid in
		( mkTok ret, ret, rest )
	| isUpper c		= let
		( ret, rest )	= span isAlphaNum s in
		( Conid ret, ret, rest )
	| isSym c		= let
		( ret, rest )	= span isSym s
		mkTok		= if ret `elem` reservedOp
					then ReservedOp else VarSym in
		( mkTok ret, ret, rest )
	| isDigit c	= let ( ret, rest ) = span isDigit s in
		( TokInteger ( read ret ), ret, rest )
	where
	isSym		= ( `elem` "!#$%&*+./<=>?@\\^|-~:" )
	isLow cc	= isLower cc || cc `elem` "_"
	isAlNum cc	= isAlphaNum cc || cc `elem` "_"
spanLex_ s			= error $ "spanLex_ failed: " ++ s

--------------------------------------------------------------------------------

updatePos :: String -> ParserMonad ()
updatePos str = do
	( idnt1, idnta, ( lns, cols ), src, buf ) <- get
	let ( nlns, ncols ) = up lns cols str
	put ( idnt1, idnta, ( nlns, ncols ), src, buf )
	where
	up l c ""		= ( l, c )
	up l _ ( '\n' : cs )	= up ( l + 1 ) 1 cs
	up l c ( '\t' : cs )	= up l ( 8 * ( c `div` 8 + 1 ) + 1 ) cs
	up l c ( _ : cs )	= up l ( c + 1 ) cs

readWhite :: String -> ( String, String )
readWhite ca@( '-' : '-' : _ )	= let
	( com, src ) = span ( /= '\n' ) ca
	( ws, rest ) = readWhite src in
	( com ++ ws, rest )
readWhite ( ' ' : cs )		=
	let ( ws, rest ) = readWhite cs in ( ' ' : ws, rest )
readWhite ( '\t' : cs )		=
	let ( ws, rest ) = readWhite cs in ( '\t' : ws, rest )
readWhite ca = ( "", ca )

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

popIndents :: ParserMonad Int
popIndents = do
	m : ms <- getIndents
	putIndents ms
	return m

putIndents :: [ Int ] -> ParserMonad ()
putIndents idnta = do
	( idnt1, _, pos, src, buf ) <- get
	put ( idnt1, idnta, pos, src, buf )

putSrc :: String -> ParserMonad ()
putSrc src = do
	( idnt1, idnta, pos, _, buf ) <- get
	put ( idnt1, idnta, pos, src, buf )

getIndents :: ParserMonad [ Int ]
getIndents = do
	( _idnt1, idnta, _pos, _src, _buf ) <- get
	return idnta

{-
peekToken :: ParserMonad Token
peekToken = do
	( _, _, _, src, _ ) <- get
	return $ one $ spanLex' src
	where
	one ( x, _, _ ) = x
-}
