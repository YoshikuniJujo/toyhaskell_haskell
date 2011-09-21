module Preprocessor (
	Parse,
	evalParse,
	popIndent,
	Token( .. ),
	prep
) where

import Lexer ( Token( .. ), lexer, lexeme )

import Control.Monad.State ( State, evalState, put, get, gets )

--------------------------------------------------------------------------------

type Parse = State ParseState

data ParseState = ParseState {
	source	:: String,
	cols	:: Int,
	indents	:: [ Int ],
	buffer	:: [ ( Token, Int ) ]
 } deriving Show

evalParse :: Parse a -> String -> a
evalParse m src = m `evalState` ParseState {
	source	= src,
	cols	= 1,
	indents	= [ ],
	buffer	= [ ]
 }

updateSrc :: ( String, String ) -> Parse ()
updateSrc ( lexed, src ) = do
	putSrc src
	updatePos lexed

putSrc :: String -> Parse ()
putSrc src = do
	stat <- get
	put stat { source = src }

updatePos :: String -> Parse ()
updatePos str = do
	stat@ParseState { cols = cs } <- get
	let ncs = up cs str
	put stat { cols = ncs }
	where
	up c ""			= c
	up _ ( '\n' : cs )	= up 1 cs
	up c ( '\t' : cs )	= up ( 8 * ( c `div` 8 + 1 ) + 1 ) cs
	up c ( _ : cs )		= up ( c + 1 ) cs

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

--------------------------------------------------------------------------------

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
	let ( t, newSrc ) = lexeme lexer src
	updateSrc newSrc
	return ( t, cs )
