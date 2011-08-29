module Lexer (
	toyLexer
) where

import Types (
	Token( .. ), ParserMonad, getSrc, putSrc, updatePos, getCols,
	pushBuf, popBuf, peekIndents, pushIndents, popIndents )
import Control.Arrow ( first )

--------------------------------------------------------------------------------

type Lexer = String -> ( Token, String, String )

small, large, digit, special, symbol :: String
small	= "abcdefghijklmnopqrstuvwxyz_"
large	= "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
digit	= "0123456789"
symbol	= "!#$%&*+./<=>?@\\^|-~:"
special = "(),;[]{}`"

reservedId, reservedOp :: [ String ]
reservedId = [
	"case", "class", "data", "default", "deriving", "do", "else", "foreign",
	 "if", "import", "in", "infix", "infixl", "infixr", "instance", "let",
	"module", "newtype", "of", "then", "type", "where", "_"
 ]
reservedOp = [ "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>" ]

toyLexer :: ( Token -> ParserMonad a ) -> ParserMonad a
toyLexer cont = preprocessor >>= cont

preprocessor :: ParserMonad Token
preprocessor = do
	t	<- addLayoutTokens
	mm	<- peekIndents
	case t of
		Indent n	-> case mm of
			Just m	| m == n	-> return $ Special ';'
				| n < m		-> do
					_ <- popIndents
					pushBuf ( t, 0 )
					return $ Special '}'
			_			-> preprocessor
		AddBrace n	-> pushIndents n >> return ( Special '{' )
		Special '}'	-> case mm of
			Just 0	-> popIndents >> return t
			_	-> error "bad close brace"
		Special '{'	-> pushIndents 0 >> return t
		TokenEOF	-> ( flip . maybe ) ( return t ) mm $ \_ -> do
			_ <- popIndents
			return $ Special '}'
		_		-> return t

addLayoutTokens :: ParserMonad Token
addLayoutTokens = do
	( t, _ ) <- lexer
	case t of
		ReservedId res | res `elem` keywords	-> do
			nt <- peekNextToken
			case nt of
				( Special '{', _ )	-> return ()
				( TokenEOF, _ )		->
					pushBuf ( AddBrace 0, 0 )
				( _, cols )		->
					pushBuf ( AddBrace cols, 0 )
			return t
		NewLine					-> do
			nt <- peekNextToken
			let ( _, cols ) = nt
			pushBuf ( Indent cols, 0 )
			addLayoutTokens
		_					-> return t
	where
	keywords = [ "where", "let", "do", "of" ]

peekNextToken :: ParserMonad ( Token, Int )
peekNextToken = lexerNoNL >>= \ret -> pushBuf ret >> return ret
	where
	lexerNoNL = lexer >>= \t -> case t of
		( NewLine, _ )	-> lexerNoNL
		_		-> return t

lexer :: ParserMonad ( Token, Int )
lexer = popBuf >>= \mtb -> ( flip . flip maybe ) return mtb $ do
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
	| c `elem` small	= spanToken ( small ++ large ++ digit ) mkTkV
	| c `elem` large	= spanToken ( small ++ large ++ digit ) Conid
	| c `elem` symbol	= spanToken symbol mkTkO
	| c `elem` digit	= spanToken digit ( TokInteger . read )
        | otherwise		= error $ "getToken failed: " ++ ca
	where
	spanToken chType f = let ( ret, rest ) = span ( `elem` chType ) ca in
		( f ret, ret, rest )
	mkTkV v	= ( if v `elem` reservedId then ReservedId else Varid ) v
	mkTkO o	= ( if o `elem` reservedOp then ReservedOp else VarSym ) o

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

lexeme :: Lexer -> Lexer
lexeme lx src = let
	( t, fin, rest )	= lx src
	( ws, rest' )		= gw rest in
	( t, fin ++ ws, rest' )
	where
	gw ca@( '-' : '-' : _ )	= let	( c, r ) = span ( /= '\n' ) ca
					( w, r' ) = gw r in
					( c ++ w, r' )
	gw ( ' ' : cs )		= first ( ' ' : ) $ gw cs
	gw ( '\t' : cs )	= first ( '\t' : ) $ gw cs
	gw ca			= ( "", ca )
