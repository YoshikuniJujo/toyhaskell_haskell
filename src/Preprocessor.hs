{-# LANGUAGE TupleSections #-}

module Preprocessor (
	Parse,
	evalParse,
	popIndent,
	Token(..),
	prep
) where

import Lexer (Token(..), lexer, lexeme)

import Control.Monad.State (State, evalState, put, get, gets)

--------------------------------------------------------------------------------

type Parse = State ParseState

data ParseState = ParseState {
	source	:: String,
	cols	:: Int,
	indents	:: [Int],
	buffer	:: [(Token, Int)]
 } deriving Show

evalParse :: Parse a -> String -> a
m `evalParse` src = m `evalState` ParseState {
	source	= src,
	cols	= 1,
	indents	= [],
	buffer	= []
 }

updateSrc :: (String, String) -> Parse ()
updateSrc (lexed, src) = do
	stat@ParseState { cols = cs } <- get
	put stat { source = src, cols = step cs lexed }
	where
	step c ""		= c
	step _ ('\n' : cs)	= step 1 cs
	step c ('\t' : cs)	= step (8 * (c `div` 8 + 1) + 1) cs
	step c (_ : cs)		= step (c + 1) cs

pushBuf :: (Token, Int) -> Parse ()
pushBuf tok = do
	stat@ParseState { buffer = buf } <- get
	put stat { buffer = tok : buf }

pushBufToken :: Token -> Parse ()
pushBufToken = pushBuf . (, 0)

popBuf :: Parse (Maybe (Token, Int))
popBuf = do
	stat@ParseState { buffer = buf } <- get
	case buf of
		t : ts	-> put stat { buffer = ts } >> return (Just t)
		_	-> return Nothing

pushIndent :: Int -> Parse ()
pushIndent m = do
	stat@ParseState { indents = ma } <- get
	put stat { indents = m : ma }

popIndent :: Parse (Maybe Int)
popIndent = do
	stat@ParseState { indents = ma } <- get
	case ma of
		m : ms	-> put stat { indents = ms } >> return (Just m)
		_	-> return Nothing

peekIndent :: Parse (Maybe Int)
peekIndent = do
	ma <- gets indents
	return $ case ma of
		m : _	-> Just m
		_	-> Nothing

--------------------------------------------------------------------------------

prep :: (Token -> Parse a) -> Parse a
prep = (preprocessor >>=)

preprocessor :: Parse Token
preprocessor = do
	tok	<- addLayout
	mm	<- peekIndent
	case tok of
		Indent n	-> case mm of
			Just m	| m == n	-> return $ Special ';'
				| n < m		-> do
					_ <- popIndent
					pushBufToken tok
					return $ Special '}'
			_			-> preprocessor
		AddBrace n	-> case mm of
			Just m	| n > m	-> pushIndent n >> return (Special '{')
			Nothing	| n > 0	-> pushIndent n >> return (Special '{')
			_		-> do	pushBufToken $ Indent n
						pushBufToken $ Special '}'
						return $ Special '{'
		Special '}'	-> case mm of
			Just 0	-> popIndent >> return tok
			_	-> error "bad close brace"
		Special '{'	-> pushIndent 0 >> return tok
		TokEOF		-> (flip . maybe) (return tok) mm $ \_ -> do
			_ <- popIndent
			return $ Special '}'
		_		-> return tok

addLayout :: Parse Token
addLayout = do
	(tok, _) <- getToken
	case tok of
		ReservedId res
			| res `elem` keywords	-> do
				next <- peekTok
				case next of
					(Special '{', _)	-> return ()
					(TokEOF, _)		->
						pushBufToken $ AddBrace 0
					(_, cs)			->
						pushBufToken $ AddBrace cs
				return tok
		NewLine				-> fmap (Indent . snd) peekTok
		_				-> return tok
	where
	keywords = ["where", "let", "do", "of"]
	peekTok = getNext >>= \next -> pushBuf next >> return next
	getNext = getToken >>= \tok -> case tok of
		(NewLine, _)	-> getNext
		_		-> return tok

infixr 2 $$
($$) :: (a -> b) -> a -> b
($$) = ($)

getToken :: Parse (Token, Int)
getToken = popBuf >>= flip maybe return $$ do
	src	<- gets source
	cs	<- gets cols
	let (tok, newSrc) = lexeme lexer src
	updateSrc newSrc
	return (tok, cs)
