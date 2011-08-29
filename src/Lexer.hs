{-# Language PackageImports #-}

module Lexer (
	lexer,
	popIndents,
) where

import Prelude hiding ( lex )
import Types ( Token( .. ), ParserMonad )
import "monads-tf" Control.Monad.State

--------------------------------------------------------------------------------

type Lexer = String -> ( Token, String, String )

white, small, large, digit, special, symbol :: String
white	= " \t\n"
small	= "abcdefghijklmnopqrstuvwxyz_"
large	= "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
digit	= "0123456789"
symbol	= "!#$%&*+./<=>?@\\^|-~:"
special = "(),;[]{}`"

reservedId, reservedOp :: [ String ]
reservedId = [
	"case", "class", "data", "default", "deriving", "do", "else", "if",
	"import", "in", "infix", "infixl", "infixr", "instance", "let",
	"module", "newtype", "of", "then", "type", "where", "_"
 ]
reservedOp = [ "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>" ]

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
	( t, _ ) <- realLexer
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
	t <- realLexer
	case t of
		( NewLine, _ )	-> realLexerNoNewLine
		_		-> return t

realLexer :: ParserMonad ( Token, Int )
realLexer = do
	tb <- popBuf
	( flip . flip maybe ) return tb $ do
		src	<- getSrc
		cols	<- getCols
		let ( t, fin, rest ) = lexeme getToken src
		updatePos fin
		putSrc rest
		return ( t, cols )

getToken :: Lexer
getToken ""			= ( TokenEOF, "", "" )
getToken ( '\n' : cs )		= ( NewLine, "\n", cs )
getToken ( '\'' : cs )		= getTokenChar cs
getToken ( '"' : cs )		= getTokenString cs
getToken ca@( c : cs )
	| c `elem` special	= ( Special c, [ c ], cs )
	| c `elem` small	= spanToken varChar mkTkV ca
	| c `elem` large	= spanToken varChar Conid ca
	| c `elem` symbol	= spanToken symbol mkTkO ca
	| c `elem` digit	= spanToken digit ( TokInteger . read ) ca
        | otherwise		= error $ "getToken failed: " ++ ca
	where
	varChar	= small ++ large ++ digit
	mkTkV v	= ( if v `elem` reservedId then ReservedId else Varid ) v
	mkTkO o	= ( if o `elem` reservedOp then ReservedOp else VarSym ) o

spanToken :: String -> ( String -> Token ) -> Lexer
spanToken cs0 f ca =
	let ( ret, rest ) = span ( `elem` cs0 ) ca in ( f ret, ret, rest )

getTokenChar :: Lexer
getTokenChar ca = let ( ret, '\'' : rest ) = span ( /= '\'' ) ca in
	( TokChar $ readChar ret, '\'' : ret ++ "'", rest )
	where
	readChar "\\n"	= '\n'
	readChar [ c ]	= c
	readChar _	= error "bad charactor literal"

getTokenString :: Lexer
getTokenString ca = let ( ret, '"' : rest ) = span ( /= '"' ) ca in
	( TokString ret, '"' : ret ++ "\"", rest )

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

lexeme :: Lexer -> Lexer
lexeme l s = let
	( t, fin, rest ) = l s
	( ws, rest' ) = readWhite rest in
	( t, fin ++ ws, rest' )

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

getCols :: ParserMonad Int
getCols = do
	( _, _, ( _, cols ), _, _ ) <- get
	return cols

getSrc :: ParserMonad String
getSrc = do
	( _, _, _, src, _ ) <- get
	return src

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
