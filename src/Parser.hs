module Parser (
	toyParse
) where

import Value
import Prelude hiding ( lex )
import Data.Char
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec hiding ( Parser, token )
import Data.Maybe

addSemi :: Int -> String -> String
addSemi _ "" = ""
addSemi n ( c1 : 'i' : 'n' : c2 : rest )
	| isSpace c1 && isSpace c2 = ' ' : ';' : 'i' : 'n' : ' ' : addSemi n rest
addSemi n0 ( '\n' : rest ) = let n1 = length $ takeWhile ( == '\t' ) rest in
	if n0 >= n1
		then ' ' : ';' : addSemi n1 rest
		else ' ' : addSemi n1 rest
addSemi n ( c : cs ) = c : addSemi n cs

data Token =
	Variable String | TokConst String | Operator String | OpenParen | CloseParen |
	Backslash | Rightarrow | Reserved String | ReservedOp String |
	TokInteger Integer | TokString String | TokBool Bool | Yet
	deriving ( Show, Eq )

tokenToValue :: Token -> Maybe Value
tokenToValue ( TokString str )	= Just $ String str
tokenToValue ( TokInteger i )	= Just $ Integer i
tokenToValue ( Variable var )	= Just $ Identifier var
tokenToValue ( TokBool b )	= Just $ Bool b
tokenToValue _			= Nothing

tokenToPattern :: Token -> Maybe Pattern
tokenToPattern ( Variable var )	= Just $ PatVar var
tokenToPattern ( TokInteger i ) = Just $ PatInteger i
tokenToPattern _		= Nothing

testToken :: Token -> Token -> Maybe Token
testToken tok0 tok1 = if tok0 == tok1 then Just tok0 else Nothing

operatorToValue :: Token -> Maybe Value
operatorToValue ( Operator op )	= Just $ Identifier op
operatorToValue _		= Nothing

reserved, reservedOp :: [ String ]
reserved = [ "let", "in", "if", "then", "else", "case", "of" ]
reservedOp = [ "=", ";", "->" ]

lex :: String -> [ Token ]
lex "" = [ ]
lex ( '(' : cs )	= OpenParen : lex cs
lex ( ')' : cs )	= CloseParen : lex cs
lex ( '\\' : cs )	= Backslash : lex cs
-- lex ( '-' : '>' : cs )	= Rightarrow : lex cs
lex ( '"' : cs )	= let ( ret, '"' : rest ) = span (/= '"') cs in
		TokString ret : lex rest
lex s@( c : cs )
	| isSpace c	= lex cs
	| isLow c	= let ( ret, rest ) = span isAlNum s in
		( if ret `elem` reserved then Reserved else Variable ) ret :
			lex rest
	| isUpper c	= let
		( ret, rest ) = span isAlphaNum s
		tok = case ret of
			"True"	-> TokBool True
			"False"	-> TokBool False
			_	-> TokConst ret in
		tok : lex rest
	| isSym c	= let ( ret, rest ) = span isSym s in
		( if ret `elem` reservedOp then ReservedOp else Operator ) ret :
			lex rest
	| isDigit c	= let ( ret, rest ) = span isDigit s in
		TokInteger ( read ret ) : lex rest
	where
	isSym cc = isSymbol cc || cc `elem` "\\-*;"
	isLow cc = isLower cc || cc `elem` "_"
	isAlNum cc = isAlphaNum cc || cc `elem` "_"
lex s			= error $ "lex failed: " ++ s

type Parser = GenParser Token ()

toyParse :: String -> Value
toyParse input =
	case parse ( do { ret <- parserInfix; eof; return ret } ) "" $
		lex $ addSemi 0 input of
		Right v	-> v
		Left v	-> Error $ show v

parserInfix :: Parser Value
parserInfix = do
	p <- parser
	f <- parserInfix'
	return $ f p

parserInfix' :: Parser ( Value -> Value )
parserInfix' = do	op <- token operatorToValue
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
	parserParens <|>
	parserLetin <|>
	parserIf <|>
	parserCase

parserLambda :: Parser Value
parserLambda = do
	_ <- token $ testToken Backslash
	vars <- many1 parserPattern
	_ <- token $ testToken $ ReservedOp "->" -- Rightarrow
	body <- parserInfix
	return $ Lambda [ ] vars body

parserLetin :: Parser Value
parserLetin = do
	pairs <- parserLet
	option ( Let pairs ) $ do
		_ <- token $ testToken $ Reserved "in"
		body <- parserInfix
		return $ Letin pairs body

parserLet :: Parser [ ( Pattern, Value ) ]
parserLet = do
	_ <- token $ testToken $ Reserved "let"
	p <- parserDef
	ps <- many $ do
		_ <- token $ testToken $ ReservedOp ";"
		parserDef
	return $ catMaybes $ p : ps

parserDef :: Parser ( Maybe ( Pattern, Value ) )
parserDef =
	option Nothing $ do
		var <- parserPattern
		args <- many parserPattern
		_ <- token $ testToken $ ReservedOp "="
		val <- parserInfix
		return $ if null args
			then Just ( var, val )
			else Just ( var,
				Lambda [ ] args val )

parserIf :: Parser Value
parserIf = do
	_ <- token $ testToken $ Reserved "if"
	test <- parserInfix
	_ <- token $ testToken $ Reserved "then"
	thn <- parserInfix
	_ <- token $ testToken $ Reserved "else"
	els <- parserInfix
	return $ If test thn els

parserCase :: Parser Value
parserCase = do
	_ <- token $ testToken $ Reserved "case"
	val <- parserInfix
	_ <- token $ testToken $ Reserved "of"
	test <- option Nothing $ do
		pattern <- parserInfix
		_ <- token $ testToken $ ReservedOp "->"
		ret <- parserInfix
		return $ Just ( pattern, ret )
	tests <- many $ do
		_ <- token $ testToken $ ReservedOp ";"
		option Nothing $ do
			pattern <- parserInfix
			_ <- token $ testToken $ ReservedOp "->"
			ret <- parserInfix
			return $ Just ( pattern, ret )
	return $ Case val $ catMaybes $ test : tests

parserParens :: Parser Value
parserParens = do
	_ <- token $ testToken OpenParen
	ret <- parserInfix
	_ <- token $ testToken CloseParen
	return ret

parserPattern :: Parser Pattern
parserPattern = parserPatternVar

parserPatternVar :: Parser Pattern
parserPatternVar = token tokenToPattern
	

token :: ( Token -> Maybe a ) -> Parser a
token test = P.token showToken posToken testTok
	where
	showToken = show
	posToken _tok = initialPos ""
	testTok = test
