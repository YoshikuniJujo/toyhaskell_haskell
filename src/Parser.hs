module Parser (
	toyParse
) where

import Prelude hiding ( lex )

import Value ( Value( .. ), Pattern( .. ), emptyEnv )
import Lexer ( Token( .. ), lex )

import Text.ParserCombinators.Parsec (
	GenParser, parse, (<|>), eof, option, optional, many, many1 )
import qualified Text.ParserCombinators.Parsec as P ( token )
import Text.ParserCombinators.Parsec.Pos ( SourcePos, newPos )
import Data.Maybe ( catMaybes )

--------------------------------------------------------------------------------

toyParse :: String -> Value
toyParse input = case parse parser "" $ lex ( newPos "" 0 0 ) input of
	Right v	-> v
	Left v	-> Error $ show v

type Parser = GenParser ( Token, SourcePos ) ()
	
token :: ( ( Token, SourcePos ) -> Maybe a ) -> Parser a
token test = P.token ( show . fst ) snd test

parser :: Parser Value
parser = parserExpr >>= \ret -> eof >> return ret

parserExpr :: Parser Value
parserExpr = parserInfixR

parserInfixR :: Parser Value
parserInfixR = do
	v1 <- parserInfixL
	option v1 $ do
		op <- token operatorRToValue
		v2 <- parserExpr
		return $ Apply ( Apply op v1 ) v2

parserInfixL :: Parser Value
parserInfixL = do
	p <- parserApply
	f <- parserInfixL'
	return $ f p

parserInfixL' :: Parser ( Value -> Value )
parserInfixL' = option id $ do
	op <- token operatorLToValue
	p <- parserApply
	f <- parserInfixL'
	return $ \v -> f $ Apply ( Apply op v ) p

parserApply :: Parser Value
parserApply = do
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

parserLambda :: Parser Value
parserLambda = do
	_ <- token $ testToken Backslash
	vars <- many1 parserPatternOp
	_ <- token $ testToken $ ReservedOp "->"
	body <- parserExpr
	return $ Lambda emptyEnv vars body

parserLetin :: Parser Value
parserLetin = do
	pairs <- parserLet
	option ( Let pairs ) $ do
		_ <- token $ testToken $ Reserved "in"
		body <- parserExpr
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
		val <- parserExpr
		return $ if null args
			then Just ( var, val )
			else Just ( var,
				Lambda emptyEnv args val )

parserIf :: Parser Value
parserIf = do
	_ <- token $ testToken $ Reserved "if"
	test <- parserExpr
	_ <- token $ testToken $ Reserved "then"
	thn <- parserExpr
	_ <- token $ testToken $ Reserved "else"
	els <- parserExpr
	return $ If test thn els

parserCase :: Parser Value
parserCase = do
	_ <- token $ testToken $ Reserved "case"
	val <- parserExpr
	_ <- token $ testToken $ Reserved "of"
	_ <- token $ testToken OpenBrace
	test <- option Nothing $ do
		pattern <- parserPatternOp
		_ <- token $ testToken $ ReservedOp "->"
		ret <- parserExpr
		return $ Just ( pattern, ret )
	tests <- many $ do
		_ <- token $ testToken $ ReservedOp ";"
		option Nothing $ do
			pattern <- parserPatternOp
			_ <- token $ testToken $ ReservedOp "->"
			ret <- parserExpr
			return $ Just ( pattern, ret )
	_ <- token $ testToken CloseBrace
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
		v <- parserExpr
		vs <- many $ do
			_ <- token $ testToken $ ReservedOp ","
			parserExpr
		return $ foldr ( \x xs -> Complex ":" [ x, xs ] ) Empty $ v : vs
	_ <- token $ testToken $ ReservedOp "]"
	return ret

parserParens :: Parser Value
parserParens = do
	_ <- token $ testToken OpenParen
	ret <- option Nil parserExpr
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

tokenToValue :: ( Token, SourcePos ) -> Maybe Value
tokenToValue ( TokChar c, _ )		= Just $ Char c
tokenToValue ( TokInteger i, _ )	= Just $ Integer i
tokenToValue ( Variable var, _ )		= Just $ Identifier var
tokenToValue ( ReservedOp "[]", _ )	= Just Empty
tokenToValue _				= Nothing

tokenToString :: ( Token, SourcePos ) -> Maybe Value
tokenToString ( TokString str, _ )	= Just $ mkStr str
	where
	mkStr ""			= Empty
	mkStr ( '\\' : 'n' : cs )	= Complex ":" [ Char '\n', mkStr cs ]
	mkStr ( c : cs )		= Complex ":" [ Char c ,mkStr cs ]
tokenToString _				= Nothing

tokenToPattern :: ( Token, SourcePos ) -> Maybe Pattern
tokenToPattern ( Variable var, _ )	= Just $ PatVar var
tokenToPattern ( TokInteger i, _ )	= Just $ PatInteger i
tokenToPattern _			= Nothing

testToken :: Token -> ( Token, SourcePos ) -> Maybe Token
testToken tok0 ( tok1, _ ) = if tok0 == tok1 then Just tok0 else Nothing

operatorLToValue, operatorRToValue :: ( Token, SourcePos ) -> Maybe Value
operatorLToValue ( Operator op, _ )
	| op `elem` opLs	= Just $ Identifier op
operatorLToValue _		= Nothing
operatorRToValue ( Operator op, _ )
	| op `elem` opRs	= Just $ Identifier op
operatorRToValue _		= Nothing

operatorToString :: ( Token, SourcePos ) -> Maybe String
operatorToString ( Operator op, _ )	= Just op
operatorToString  _			= Nothing

opLs, opRs :: [ String ]
opLs = [ "+", "*", "-", "==" ]
opRs = [ ":", ">>" ]

getTokConst :: ( Token, SourcePos ) -> Maybe String
getTokConst ( TokConst name, _ )	= Just name
getTokConst _				= Nothing
