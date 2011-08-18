module Parser (
	toyParse,
	addSemi,
	eraseImport
) where

import Value
import Prelude hiding ( lex )
import Data.Char
import Text.ParserCombinators.Parsec.Pos
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec hiding ( Parser, token )
import Data.Maybe
import Data.List

addSemi :: Int -> String -> String
addSemi _ "" = ""
addSemi n ( c1 : 'i' : 'n' : c2 : rest )
	| isSpace c1 && isSpace c2 = ' ' : ';' : 'i' : 'n' : ' ' : addSemi n rest
addSemi n0 ( '\n' : rest ) = let n1 = length $ takeWhile ( == '\t' ) rest in
	if n0 >= n1
		then ' ' : ';' : '\n' : addSemi n1 rest
		else '\n' : addSemi n1 rest
addSemi n ( c : cs ) = c : addSemi n cs

eraseImport :: String -> String
eraseImport = unlines . filter ( not . ( "import " `isPrefixOf` ) ) . lines

data Token =
	Variable String | TokConst String | Operator String | OpenParen |
	CloseParen | Backslash | Reserved String | ReservedOp String |
	TokInteger Integer | TokChar Char | TokString String | TokBool Bool | Yet
	deriving ( Show, Eq )

tokenToValue :: Token -> Maybe Value
tokenToValue ( TokChar c )		= Just $ Char c
tokenToValue ( TokString str )		= Nothing
tokenToValue ( TokInteger i )		= Just $ Integer i
tokenToValue ( Variable var )		= Just $ Identifier var
tokenToValue ( TokBool b )		= Just $ Bool b
tokenToValue ( ReservedOp "[]" )	= Just Empty
tokenToValue _				= Nothing

tokenToString :: Token -> Maybe Value
tokenToString ( TokString str )	= Just $ mkStr str
	where
	mkStr ""			= Empty
	mkStr ( '\\' : 'n' : cs )	= Complex ":" [ Char '\n', mkStr cs ]
	mkStr ( c : cs )		= Complex ":" [ Char c ,mkStr cs ]
tokenToString _				= Nothing

tokenToPattern :: Token -> Maybe Pattern
tokenToPattern ( Variable var )		= Just $ PatVar var
tokenToPattern ( TokInteger i )		= Just $ PatInteger i
tokenToPattern _			= Nothing

testToken :: Token -> Token -> Maybe Token
testToken tok0 tok1 = if tok0 == tok1 then Just tok0 else Nothing

operatorToValue, operatorLToValue :: Token -> Maybe Value
operatorToValue ( Operator op )	= Just $ Identifier op
operatorToValue _		= Nothing
operatorLToValue ( Operator op )
	| op `elem` opLs	= Just $ Identifier op
operatorLToValue _		= Nothing
operatorRToValue ( Operator op )
	| op `elem` opRs	= Just $ Identifier op
operatorRToValue _		= Nothing

operatorToString :: Token -> Maybe String
operatorToString ( Operator op )	= Just op
operatorToString  _			= Nothing

opLs, opRs :: [ String ]
opLs = [ "+", "*", "-", "==" ]
opRs = [ ":", ">>" ]

getTokConst :: Token -> Maybe String
getTokConst ( TokConst name )	= Just name
getTokConst _			= Nothing

reserved, reservedOp :: [ String ]
reserved = [ "let", "in", "if", "then", "else", "case", "of" ]
reservedOp = [ "=", ";", "->", "[]", "[", "]", "," ]

lex :: String -> [ Token ]
lex "" = [ ]
lex ( '-' : '-' : cs )		= lex $ dropWhile ( /= '\n' ) cs
lex ( '[' : cs )		= ReservedOp "[" : lex cs
lex ( ']' : cs )		= ReservedOp "]" : lex cs
lex ( '(' : cs )		= OpenParen : lex cs
lex ( ')' : cs )		= CloseParen : lex cs
lex ( '\\' : cs )		= Backslash : lex cs
lex ( '\'' : '\\' : 'n' : '\'' : cs ) = TokChar '\n' : lex cs
lex ( '\'' : c : '\'' : cs )	= TokChar c : lex cs
lex ( '"' : cs )		= let ( ret, '"' : rest ) = span (/= '"') cs in
		TokString ret : lex rest
lex s@( c : cs )
	| isSpace c		= lex cs
	| isLow c		= let ( ret, rest ) = span isAlNum s in
		( if ret `elem` reserved then Reserved else Variable ) ret :
			lex rest
	| isUpper c		= let
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
	isSym cc = isSymbol cc || cc `elem` "\\-*;:,"
	isLow cc = isLower cc || cc `elem` "_"
	isAlNum cc = isAlphaNum cc || cc `elem` "_"
lex s			= error $ "lex failed: " ++ s

type Parser = GenParser Token ()

toyParse :: String -> Value
toyParse input =
	case parse ( do { ret <- parserInfix; eof; return ret } ) "" $
		lex $ eraseImport $ addSemi 0 input of
		Right v	-> v
		Left v	-> Error $ show v

parserInfix :: Parser Value
parserInfix = do
	v1 <- parserInfixL
	option v1 $ do
		op <- token operatorRToValue
		v2 <- parserInfix
		return $ Apply ( Apply op v1 ) v2

parserInfixL :: Parser Value
parserInfixL = do
	p <- parser
	f <- parserInfix'
	return $ f p

parserInfix' :: Parser ( Value -> Value )
parserInfix' = do	op <- token operatorLToValue
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
	token tokenToString <|>
	token tokenToValue <|>
	parserLambda <|>
	parserParens <|>
	parserLetin <|>
	parserIf <|>
	parserCase <|>
	parserComplex <|>
	parserList

parserNil :: Parser Value
parserNil = do
	_ <- token $ testToken OpenParen
	_ <- token $ testToken CloseParen
	return Nil

parserLambda :: Parser Value
parserLambda = do
	_ <- token $ testToken Backslash
	vars <- many1 parserPatternOp
	_ <- token $ testToken $ ReservedOp "->"
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
	optional $ token ( testToken $ ReservedOp ";" ) >> return ()
	p <- parserDef
	ps <- many $ do
		_ <- token $ testToken $ ReservedOp ";"
		parserDef
	return $ catMaybes $ p : ps

parserDef :: Parser ( Maybe ( Pattern, Value ) )
parserDef =
	option Nothing $ do
		var <- parserPatternOp
		args <- many parserPatternOp
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
		pattern <- parserPatternOp
		_ <- token $ testToken $ ReservedOp "->"
		ret <- parserInfix
		return $ Just ( pattern, ret )
	tests <- many $ do
		_ <- token $ testToken $ ReservedOp ";"
		option Nothing $ do
			pattern <- parserPatternOp
			_ <- token $ testToken $ ReservedOp "->"
			ret <- parserInfix
			return $ Just ( pattern, ret )
	return $ Case val $ catMaybes $ test : tests

parserComplex :: Parser Value
parserComplex = do
	name <- token getTokConst
	bodys <- many parserAtom
	return $ Complex name bodys

parserList :: Parser Value
parserList = do
	_ <- token $ testToken $ ReservedOp "["
	ret <- option Empty $ do
		v <- parserInfix
		vs <- many $ do
			_ <- token $ testToken $ ReservedOp ","
			parserInfix
		return $ foldr ( \x xs -> Complex ":" [ x, xs ] ) Empty $ v : vs
	_ <- token $ testToken $ ReservedOp "]"
	return ret

parserParens :: Parser Value
parserParens = do
	_ <- token $ testToken OpenParen
	ret <- option Nil $ parserInfix
	_ <- token $ testToken CloseParen
	return ret

parserPattern :: Parser Pattern
parserPattern = parserPatternVar <|> parserPatternComplex <|> parserPatternEmpty

parserPatternVar :: Parser Pattern
parserPatternVar = token tokenToPattern

parserPatternEmpty :: Parser Pattern
parserPatternEmpty = do
	_ <- token $ testToken $ ReservedOp "["
	_ <- token $ testToken $ ReservedOp "]"
	return PatEmpty

parserPatternOp :: Parser Pattern
parserPatternOp = do
	p1 <- parserPattern
	f <- parserPatternOp'
	return $ f p1

parserPatternOp' :: Parser ( Pattern -> Pattern )
parserPatternOp' = do
		op <- token operatorToString
		p1 <- parserPattern
		f <- parserPatternOp'
		return $ \p -> PatConst op [ p, f p1 ]
	<|> return id

parserPatternComplex :: Parser Pattern
parserPatternComplex = do
	name <- token getTokConst
	bodys <- many parserPattern
	return $ PatConst name bodys
	
token :: ( Token -> Maybe a ) -> Parser a
token test = P.token showToken posToken testTok
	where
	showToken = show
	posToken _tok = initialPos ""
	testTok = test
