module Parser (
	toyParse
) where

import Prelude hiding ( lex )

import Value ( Value( .. ), Pattern( .. ), emptyEnv )
import Preprocessor ( Token( .. ), lex, prep )
import BuildExpression ( buildExprParser, Assoc( .. ) )

import Text.ParserCombinators.Parsec (
	GenParser, runParser, (<|>), eof, option, optional, many, many1,
	getState )
import qualified Text.ParserCombinators.Parsec as P ( token )
import Text.ParserCombinators.Parsec.Pos ( SourcePos, initialPos )
import Data.Maybe ( catMaybes )
import Data.Function ( on )
import Data.Char

--------------------------------------------------------------------------------

type Parser = GenParser ( Token, SourcePos ) String
	
token :: ( ( Token, SourcePos ) -> Maybe a ) -> Parser a
token = P.token ( show . fst ) snd

toyParse :: FilePath -> String -> String -> Value
toyParse opLst fn input =
	case runParser parser opLst fn $ prep 0 [ ] $ lex ( initialPos fn ) input of
		Right v	-> v
		Left v	-> Error $ show v

parser :: Parser Value
parser = parserExpr >>= \ret -> eof >> return ret

parserExpr :: Parser Value
parserExpr = do
	opLst <- getState
	let opTable = map ( uncurry3 mkAssoc . readOpTable ) $
		concatMap prepOpTable $ lines opLst
	buildExprParser token ( on (==) fst ) opTable parserApply

uncurry3 :: ( a -> b -> c -> d ) -> ( a, b, c ) -> d
uncurry3 f ( x, y, z ) = f x y z

mkAssoc :: String -> Int -> Assoc ->
	( ( Token, SourcePos ), Value -> Value -> Value, Int, Assoc )
mkAssoc op power assoc =
	( ( Operator op, initialPos "" ), Apply . Apply ( Identifier op ),
		power, assoc )

parserApply :: Parser Value
parserApply = do
	a <- parserAtom
	f <- parserApply'
	return $ f a

parserApply' :: Parser ( Value -> Value )
parserApply' = option id $ do
	a <- parserAtom
	f <- parserApply'
	return $ \v -> f $ Apply v a

parserAtom :: Parser Value
parserAtom =
	token tokenToValue <|>
	parserParens <|>
	parserLambda <|>
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
--	return $ If test thn els
	return $ Case test [ ( PatConst "True" [ ], thn ),
		( PatConst "False" [ ], els ) ]

parserCase :: Parser Value
parserCase = do
	_ <- token $ testToken $ Reserved "case"
	val <- parserExpr
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
		ret <- parserExpr
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
tokenToValue ( TokString str, _ )	= Just $ mkStr str
	where
	mkStr ""			= Empty
	mkStr ( '\\' : 'n' : cs )	= Complex ":" [ Char '\n', mkStr cs ]
	mkStr ( c : cs )		= Complex ":" [ Char c ,mkStr cs ]
tokenToValue _				= Nothing

tokenToPattern :: ( Token, SourcePos ) -> Maybe Pattern
tokenToPattern ( Variable var, _ )	= Just $ PatVar var
tokenToPattern ( TokInteger i, _ )	= Just $ PatInteger i
tokenToPattern _			= Nothing

testToken :: Token -> ( Token, SourcePos ) -> Maybe Token
testToken tok0 ( tok1, _ ) = if tok0 == tok1 then Just tok0 else Nothing

operatorToString :: ( Token, SourcePos ) -> Maybe String
operatorToString ( Operator op, _ )	= Just op
operatorToString  _			= Nothing

getTokConst :: ( Token, SourcePos ) -> Maybe String
getTokConst ( TokConst name, _ )	= Just name
getTokConst _				= Nothing

--------------------------------------------------------------------------------

prepOpTable :: String -> [ String ]
prepOpTable  str = map ( \op -> fix ++ " " ++ power ++ " " ++ op ) ops
	where
	fix : power : ops = sep str
	sep "" = [ ]
	sep [ x ] = [ [ x ] ]
	sep ( ',' : cs ) = "" : sep ( dropWhile isSpace cs )
	sep ( c : cs )
		| isSpace c	= "" : sep ( dropWhile ( `elem` " ,\t" ) cs )
		| otherwise	= ( c : r ) : rs
		where
		r : rs = sep cs

readOpTable :: String -> ( String, Int, Assoc )
readOpTable str = ( op, read power, assoc )
	where
	[ fix, power, op_ ] = words str
	assoc = case fix of
		"infix"		-> AssocNone
		"infixr"	-> AssocRight
		"infixl"	-> AssocLeft
		_		-> error "bad"
	op = case op_ of
		'`' : o	-> init o
		_	-> op_
