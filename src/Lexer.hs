module Lexer (
	Parse,
	evalParse,
	popIndent,
	Token( .. ),
	prep
) where

import Control.Arrow ( first )
import Control.Monad.State ( State, evalState, put, get, gets )

--------------------------------------------------------------------------------

data Token =
	TokInteger Integer	|
	TokChar Char		|
	TokString String	|
	Special Char		|
	ReservedOp String	|
	ReservedId String	|
	VarSym String		|
	ConSym String		|
	VarId String		|
	ConId String		|
	NewLine			|
	TokEOF			|
	AddBrace Int		|
	Indent Int
	deriving ( Show, Eq )

small, large, symbol, digit, special :: String
small	= "abcdefghijklmnopqrstuvwxyz_"
large	= "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
symbol	= "!#$%&*+./<=>?@\\^|-~:"
digit	= "0123456789"
special = "(),;[]`{}"

reservedId, reservedOp :: [ String ]
reservedId = [
	"case", "class", "data", "default", "deriving", "do", "else", "foreign",
	"if", "import", "in", "infix", "infixl", "infixr", "instance", "let",
	"module", "newtype", "of", "then", "type", "where", "_"
 ]
reservedOp = [ "..", ":", "::", "=", "\\", "|", "<-", "->", "@", "~", "=>" ]

prep :: ( Token -> Parse a ) -> Parse a
prep = ( preprocessor >>= )

preprocessor :: Parse Token
preprocessor = do
	t	<- addLayoutTokens
	mm	<- peekIndent
	case t of
		Indent n	-> case mm of
			Just m	| m == n	-> return $ Special ';'
				| n < m		-> do
					_ <- popIndent
					pushBuf ( t, 0 )
					return $ Special '}'
			_			-> preprocessor
		AddBrace n	-> case mm of
			Just m	| n > m	-> pushIndent n >> return ( Special '{' )
			Nothing	| n > 0	-> pushIndent n >> return ( Special '{' )
			_		-> do	pushBuf ( Indent n, 0 )
						pushBuf ( Special '}', 0 )
						return $ Special '{'
		Special '}'	-> case mm of
			Just 0	-> popIndent >> return t
			_	-> error "bad close brace"
		Special '{'	-> pushIndent 0 >> return t
		TokEOF		-> ( flip . maybe ) ( return t ) mm $ \_ -> do
			_ <- popIndent
			return $ Special '}'
		_		-> return t

addLayoutTokens :: Parse Token
addLayoutTokens = do
	( t, _ ) <- getToken
	case t of
		ReservedId res | res `elem` keywords	-> do
			nt <- peekNextToken
			case nt of
				( Special '{', _ )	-> return ()
				( TokEOF, _ )		->
					pushBuf ( AddBrace 0, 0 )
				( _, cs )		->
					pushBuf ( AddBrace cs, 0 )
			return t
		NewLine					->
			fmap ( Indent . snd ) peekNextToken
		_					-> return t
	where
	keywords = [ "where", "let", "do", "of" ]

peekNextToken :: Parse ( Token, Int )
peekNextToken = lexerNoNL >>= \ret -> pushBuf ret >> return ret
	where
	lexerNoNL = getToken >>= \t -> case t of
		( NewLine, _ )	-> lexerNoNL
		_		-> return t

getToken :: Parse ( Token, Int )
getToken = popBuf >>= \mt -> ( flip . flip maybe ) return mt $ do
	src	<- gets source
	cs	<- gets cols
	let ( t, fin, rest ) = lexeme lexer src
	updatePos fin
	putSrc rest
	return ( t, cs )

type Lexer = String -> ( Token, String, String )

lexer :: Lexer
lexer ""			= ( TokEOF, "", "" )
lexer ( '\n' : cs )		= ( NewLine, "\n", cs )
lexer ( '\'' : cs )		= lexerChar cs
lexer ( '"' : cs )		= lexerString cs
lexer ca@( c : cs )
	| c `elem` special	= ( Special c, [ c ], cs )
	| c `elem` small	= spanToken ( small ++ large ++ digit ) mkTkV
	| c `elem` large	= spanToken ( small ++ large ++ digit ) ConId
	| c `elem` ":"		= spanToken symbol mkTkC
	| c `elem` symbol	= spanToken symbol mkTkO
	| c `elem` digit	= spanToken digit ( TokInteger . read )
        | otherwise		= error $ "lexer failed: " ++ ca
	where
	spanToken chType f = let ( ret, rest ) = span ( `elem` chType ) ca in
		( f ret, ret, rest )
	mkTkV v	= ( if v `elem` reservedId then ReservedId else VarId ) v
	mkTkO o	= ( if o `elem` reservedOp then ReservedOp else VarSym ) o
	mkTkC o = ( if o `elem` reservedOp then ReservedOp else ConSym ) o

lexerChar :: Lexer
lexerChar ca = let ( ret, '\'' : rest ) = span ( /= '\'' ) ca in
	( TokChar $ readChar ret, '\'' : ret ++ "'", rest )
	where
	readChar "\\n"	= '\n'
	readChar [ c ]	= c
	readChar _	= error "bad charactor literal"

lexerString :: Lexer
lexerString ca = let ( ret, '"' : rest ) = span ( /= '"' ) ca in
	( TokString ret, '"' : ret ++ "\"", rest )

lexeme :: Lexer -> Lexer
lexeme lx src = let
	( t, fin, rest )	= lx src
	( ws, rest' )		= gw rest in
	( t, fin ++ ws, rest' )
	where
	gw ca@( '-' : '-' : _ )	= first ( c ++ ) $ gw r
		where ( c, r ) = span ( /= '\n' ) ca
	gw ( ' ' : cs )		= first ( ' ' : ) $ gw cs
	gw ( '\t' : cs )	= first ( '\t' : ) $ gw cs
	gw ca			= ( "", ca )

--------------------------------------------------------------------------------

type Parse = State ParseState

data ParseState = ParseState {
	indents	:: [ Int ],
	lns	:: Int,
	cols	:: Int,
	source	:: String,
	buffer	:: [ ( Token, Int ) ]
 } deriving Show

evalParse :: Parse a -> String -> a
evalParse m src = m `evalState` ParseState {
	indents	= [ ],
	lns	= 1,
	cols	= 1,
	source	= src,
	buffer	= [ ]
 }

putSrc :: String -> Parse ()
putSrc src = do
	stat <- get
	put stat { source = src }

updatePos :: String -> Parse ()
updatePos str = do
	stat@ParseState { lns = ls, cols = cs } <- get
	let ( nls, ncs ) = up ls cs str
	put stat { lns = nls, cols = ncs }
	where
	up l c ""		= ( l, c )
	up l _ ( '\n' : cs )	= up ( l + 1 ) 1 cs
	up l c ( '\t' : cs )	= up l ( 8 * ( c `div` 8 + 1 ) + 1 ) cs
	up l c ( _ : cs )	= up l ( c + 1 ) cs

pushBuf :: ( Token, Int ) -> Parse ()
pushBuf t = do
	stat@ParseState { buffer = buf } <- get
	put stat { buffer = t : buf }

popBuf :: Parse ( Maybe ( Token, Int ) )
popBuf = do
	stat@ParseState { buffer = buf } <- get
	case buf of
		t : ts	-> put stat { buffer = ts } >> return ( Just t )
		_	-> return Nothing

pushIndent :: Int -> Parse ()
pushIndent m = do
	stat@ParseState { indents = ma } <- get
	put stat { indents = m : ma }

popIndent :: Parse ( Maybe Int )
popIndent = do
	stat@ParseState { indents = ma } <- get
	case ma of
		m : ms	-> put stat { indents = ms } >> return ( Just m )
		_	-> return Nothing

peekIndent :: Parse ( Maybe Int )
peekIndent = do
	ma <- gets indents
	case ma of
		m : _	-> return $ Just m
		_	-> return Nothing
