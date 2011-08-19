module Parser (
	toyParse
) where

import Prelude hiding ( lex )

import Value ( Value( .. ), Pattern( .. ), emptyEnv )
import Lexer ( Token( .. ), lex )
import BuildExpression

import Text.ParserCombinators.Parsec (
	GenParser, parse, (<|>), eof, option, optional, many, many1 )
import qualified Text.ParserCombinators.Parsec as P ( token )
import Text.ParserCombinators.Parsec.Pos ( SourcePos, newPos, initialPos )
import Text.ParserCombinators.Parsec.Expr
import Data.Maybe ( catMaybes )

--------------------------------------------------------------------------------

toyParse :: String -> Value
toyParse input = case parse parser "" $ lex ( newPos "" 0 0 ) input of
	Right v	-> v
	Left v	-> Error $ show v

type Parser = GenParser ( Token, SourcePos ) ()
	
token :: ( ( Token, SourcePos ) -> Maybe a ) -> Parser a
token = P.token ( show . fst ) snd

parser :: Parser Value
parser = parserNew >>= \ret -> eof >> return ret

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
	body <- parserNew
	return $ Lambda emptyEnv vars body

parserLetin :: Parser Value
parserLetin = do
	pairs <- parserLet
	option ( Let pairs ) $ do
		_ <- token $ testToken $ Reserved "in"
		body <- parserNew
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
		val <- parserNew
		return $ if null args
			then Just ( var, val )
			else Just ( var,
				Lambda emptyEnv args val )

parserIf :: Parser Value
parserIf = do
	_ <- token $ testToken $ Reserved "if"
	test <- parserNew
	_ <- token $ testToken $ Reserved "then"
	thn <- parserNew
	_ <- token $ testToken $ Reserved "else"
	els <- parserNew
	return $ If test thn els

parserCase :: Parser Value
parserCase = do
	_ <- token $ testToken $ Reserved "case"
	val <- parserNew
	_ <- token $ testToken $ Reserved "of"
	_ <- token $ testToken OpenBrace
	test <- dup
	tests <- many $ do
		_ <- token $ testToken $ ReservedOp ";"
		dup
	_ <- token $ testToken CloseBrace
	return $ Case val $ catMaybes $ test : tests
	where
	dup = option Nothing $ do
		pattern <- parserPatternOp
		_ <- token $ testToken $ ReservedOp "->"
		ret <- parserNew
		return $ Just ( pattern, ret )

parserComplex :: Parser Value
parserComplex = do
	name <- token getTokConst
	bodys <- many parserAtom
	return $ Complex name bodys

parserList :: Parser Value
parserList = do
	_ <- token $ testToken $ ReservedOp "["
	ret <- option Empty $ do
		v <- parserNew
		vs <- many $ do
			_ <- token $ testToken $ ReservedOp ","
			parserNew
		return $ foldr ( \x xs -> Complex ":" [ x, xs ] ) Empty $ v : vs
	_ <- token $ testToken $ ReservedOp "]"
	return ret

parserParens :: Parser Value
parserParens = do
	_ <- token $ testToken OpenParen
	ret <- option Nil parserNew
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

operatorToString :: ( Token, SourcePos ) -> Maybe String
operatorToString ( Operator op, _ )	= Just op
operatorToString  _			= Nothing

parserNew :: Parser Value
parserNew = buildExprParser ( show . fst ) snd fst table parserApply

table :: OpTable ( Token, SourcePos ) () Value
table = map ( uncurry3 mkAssoc ) [
	( ">>", 1, AssocLeft ),
	( "==", 4, AssocNone ),
	( ":", 5, AssocRight ),
	( "+", 6, AssocLeft ),
	( "-", 6, AssocLeft ),
	( "*", 7, AssocLeft ),
	( "div", 7, AssocLeft )
 ]

uncurry3 :: ( a -> b -> c -> d ) -> ( a, b, c ) -> d
uncurry3 f ( x, y, z ) = f x y z

mkAssoc ::
	String -> Int -> Assoc -> ( ( Token, SourcePos ), Value -> Value -> Value, Int, Assoc )
mkAssoc op power assoc =
	( ( Operator op, initialPos "" ), Apply . Apply ( Identifier op ), power, assoc )

getTokConst :: ( Token, SourcePos ) -> Maybe String
getTokConst ( TokConst name, _ )	= Just name
getTokConst _				= Nothing
