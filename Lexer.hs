module Lexer (
	toyParse
) where

import Value
import Prelude hiding ( lex )
import Data.Char
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec hiding ( Parser, token )

data Token =
	Variable String | Operator String | OpenParen | CloseParen |
	Backslash | Rightarrow |
	TokInteger Integer | TokString String
	deriving ( Show, Eq )

tokenToValue :: Token -> Maybe Value
tokenToValue ( TokString str )	= Just $ String str
tokenToValue ( TokInteger i )	= Just $ Integer i
tokenToValue ( Variable var )	= Just $ Identifier var
tokenToValue _			= Nothing

testToken :: Token -> Token -> Maybe Token
testToken tok0 tok1 = if tok0 == tok1 then Just tok0 else Nothing

variableToStr :: Token -> Maybe String
variableToStr ( Variable name )	= Just name
variableToStr _			= Nothing

operatorToValue :: Token -> Maybe Value
operatorToValue ( Operator op )	= Just $ Identifier op
operatorToValue _		= Nothing

lex :: String -> [ Token ]
lex "" = [ ]
lex ( '(' : cs )	= OpenParen : lex cs
lex ( ')' : cs )	= CloseParen : lex cs
lex ( '\\' : cs )	= Backslash : lex cs
lex ( '-' : '>' : cs )	= Rightarrow : lex cs
lex ( '"' : cs )	= let ( ret, '"' : rest ) = span (/= '"') cs in
		TokString ret : lex rest
lex s@( c : cs )
	| isSpace c	= lex cs
	| isLower c	= let ( ret, rest ) = span isAlphaNum s in
		Variable ret : lex rest
	| isSym c	= let ( ret, rest ) = span isSym s in
		Operator ret : lex rest
	| isDigit c	= let ( ret, rest ) = span isDigit s in
		TokInteger ( read ret ) : lex rest
	where
	isSym c = isSymbol c || c `elem` "\\-"
lex s			= error $ "lex failed: " ++ s

type Parser = GenParser Token ()

toyParse :: String -> Either ParseError Value
toyParse = parse ( do { ret <- parserInfix; eof; return ret } ) "" . lex

parserInfix :: Parser Value
parserInfix = do
	p <- parser
	f <- parserInfix'
	return $ f p

parserInfix' :: Parser ( Value -> Value )
parserInfix' = do	op <- token $ operatorToValue
			p <- parser
			f <- parserInfix'
			return $ \v -> f $ Apply ( Apply op v ) p
		<|> return id

parser :: Parser Value
parser = do
	a <- parserAtom
	f <- parser'
	return $ f a

parser' :: Parser ( Value -> Value )
parser' = do	a <- parserAtom
		f <- parser'
		return $ \v -> f $ Apply v a
	<|> return id

parserAtom :: Parser Value
parserAtom =
	token tokenToValue <|>
	parserLambda <|>
	parserParens

parserLambda :: Parser Value
parserLambda = do
	token $ testToken Backslash
	vars <- many1 $ token variableToStr
	token $ testToken Rightarrow
	body <- parserInfix
	return $ Lambda [ ] vars body

parserParens :: Parser Value
parserParens = do
	token $ testToken $ OpenParen
	ret <- parserInfix
	token $ testToken $ CloseParen
	return ret

token :: ( Token -> Maybe a ) -> Parser a
token test = P.token showToken posToken testToken
	where
	showToken tok = show tok
	posToken tok = initialPos ""
	testToken tok = test tok
