module Parser (
	toyParse,
	getOpTable
) where

import Types ( Value( .. ), Pattern( .. ), Token( .. ), OpTable, emptyEnv )
import BuildExpression ( buildExprParser, Assoc( .. ), Op )

import Text.ParserCombinators.Parsec (
	GenParser, runParser, (<|>), eof, option, optional, many, many1, sepBy1,
	sepBy, getState )
import qualified Text.ParserCombinators.Parsec as P ( token )
import Text.ParserCombinators.Parsec.Pos ( SourcePos, initialPos )
import Data.Maybe ( catMaybes )
import Data.Function ( on )
import Data.Char ( isSpace )

--------------------------------------------------------------------------------

type Parser = GenParser ( Token, SourcePos ) OpTable
	
token :: ( ( Token, SourcePos ) -> Maybe a ) -> Parser a
token = P.token ( show . fst ) snd

tok :: Token -> Parser ()
tok = ( >> return () ) . token . eq
	where eq x y = if x == fst y then Just () else Nothing

toyParse :: OpTable -> String -> [ ( Token, SourcePos ) ] -> Value
toyParse opTable fn input = case runParser parser opTable fn input of
	Right v	-> v
	Left v	-> Error $ show v

parser :: Parser Value
parser = parserExpr >>= \ret -> eof >> return ret

parserExpr :: Parser Value
parserExpr = do
	opTbl <- fmap ( map buildOpVal ) getState
	buildExprParser token ( on (==) fst ) opTbl parserApply

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
	token tokToVal <|>
	parserParens <|>
	parserLambda <|>
	parserLetin <|>
	parserIf <|>
	parserCase <|>
	parserComplex <|>
	parserList
	where
	tokToVal ( TokChar c, _ )	= Just $ Char c
	tokToVal ( TokInteger i, _ )	= Just $ Integer i
	tokToVal ( Variable var, _ )	= Just $ Identifier var
	tokToVal ( TokString str, _ )	= Just $ mkStr str
	tokToVal _			= Nothing
	mkStr ""			= Empty
	mkStr ( '\\' : 'n' : cs )	= Complex ":" [ Char '\n', mkStr cs ]
	mkStr ( '\\' : '\\' : cs )	= Complex ":" [ Char '\\', mkStr cs ]
	mkStr ( c : cs )		= Complex ":" [ Char c ,mkStr cs ]

parserParens :: Parser Value
parserParens = do
	tok $ Special '('
	ret <- option Nil parserExpr
	tok $ Special ')'
	return ret

parserLambda :: Parser Value
parserLambda = do
	tok Backslash
	vars <- many1 parserPattern
	tok $ ReservedOp "->"
	body <- parserExpr
	return $ Lambda emptyEnv vars body

parserLetin :: Parser Value
parserLetin = do
	pairs <- parserLet
	option ( Let pairs ) $ do
		tok $ Reserved "in"
		body <- parserExpr
		return $ Letin pairs body

parserLet :: Parser [ ( Pattern, Value ) ]
parserLet = do
	tok $ Reserved "let"
	optional $ tok $ ReservedOp ";"
	p <- parserDef
	ps <- many $ do
		tok $ ReservedOp ";"
		parserDef
	return $ catMaybes $ p : ps

parserDef :: Parser ( Maybe ( Pattern, Value ) )
parserDef = option Nothing $ do
	var <- parserPattern
	args <- many parserPattern
	tok $ ReservedOp "="
	val <- parserExpr
	return $ if null args
		then Just ( var, val )
		else Just ( var, Lambda emptyEnv args val )

parserIf :: Parser Value
parserIf = do
	tok $ Reserved "if"
	test <- parserExpr
	tok $ Reserved "then"
	thn <- parserExpr
	tok $ Reserved "else"
	els <- parserExpr
	return $ Case test [ ( PatConst "True" [ ], thn ),
		( PatConst "False" [ ], els ) ]

parserCase :: Parser Value
parserCase = do
	tok $ Reserved "case"
	val	<- parserExpr
	tok $ Reserved "of"
	tok $ Special '{'
	tests	<- flip sepBy1 ( tok $ ReservedOp ";" ) $ option Nothing $ do
		pattern	<- parserPattern
		tok $ ReservedOp "->"
		ret	<- parserExpr
		return $ Just ( pattern, ret )
	tok $ Special '}'
	return $ Case val $ catMaybes tests

parserComplex :: Parser Value
parserComplex = do
	name	<- token getTokConst
	bodys	<- many parserAtom
	return $ Complex name bodys

getTokConst :: ( Token, SourcePos ) -> Maybe String
getTokConst ( TokConst name, _ )	= Just name
getTokConst _				= Nothing

parserList :: Parser Value
parserList = do
	tok $ ReservedOp "["
	ret	<- do
		vs	<- sepBy parserExpr $ tok $ ReservedOp ","
		return $ foldr ( \x xs -> Complex ":" [ x, xs ] ) Empty vs
	tok $ ReservedOp "]"
	return ret

parserPattern :: Parser Pattern
parserPattern = do
	opTable <- fmap ( map buildOpPat ) getState
	buildExprParser token ( on (==) fst ) opTable parserPatternAtom

parserPatternAtom :: Parser Pattern
parserPatternAtom =
	token tokToPat <|> parserPatternComplex <|> parserPatternList
	where
	tokToPat ( Variable var, _ )	= Just $ PatVar var
	tokToPat ( TokInteger i, _ )	= Just $ PatInteger i
	tokToPat ( Reserved "_", _ )	= Just PatUScore
	tokToPat _			= Nothing

parserPatternComplex :: Parser Pattern
parserPatternComplex = do
	name	<- token getTokConst
	bodys	<- many parserPatternAtom
	return $ PatConst name bodys

parserPatternList :: Parser Pattern
parserPatternList = do
	tok ( ReservedOp "[" )
	ret <- do
		vs	<- sepBy parserPattern $ tok $ ReservedOp ","
		return $ foldr ( \x xs -> PatConst ":" [ x, xs ] ) PatEmpty vs
	tok ( ReservedOp "]" )
	return ret

fromRawOp :: ( String -> a -> a -> a ) -> ( String, Int, Assoc ) ->
	Op ( Token, SourcePos ) a
fromRawOp f ( op, power, assoc ) =
	( ( Operator op, initialPos "" ), f op, power, assoc )

buildOpVal :: ( String, Int, Assoc ) -> Op ( Token, SourcePos ) Value
buildOpVal = fromRawOp $ (.) Apply . Apply . Identifier

buildOpPat :: ( String, Int, Assoc ) -> Op ( Token, SourcePos ) Pattern
buildOpPat = fromRawOp $ \op -> (.) ( PatConst op ) . flip (.) ( : [] ) . (:)

-------------------------------------------------------------------------------

getOpTable :: String -> OpTable
getOpTable = map readOpTable . concatMap prepOpTable . lines

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
