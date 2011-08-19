module Main where

import Text.ParserCombinators.Parsec hiding ( Parser, token )
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Pos
import Text.ParserCombinators.Parsec.Expr
import Data.Char
import Interact

main :: IO ()
main = runLoop "expr" () $ \() input -> do
	putStrLn $ either show ( show . eval ) $ parseExpr $ lexer input

data Op = Plus | Minus | Multi | Div | Other String deriving ( Eq, Show )
data Token = Integer Integer | Op Op | TokError String deriving ( Eq, Show )
data Expr = Num Integer | Expr Op Expr Expr deriving Show

lexer :: String -> [ Token ]
lexer ""		= [ ]
{-
lexer ( '+' : cs )	= Op Plus : lexer cs
lexer ( '-' : cs )	= Op Minus : lexer cs
lexer ( '*' : cs )	= Op Multi :  lexer cs
lexer ( '/' : cs )	= Op Div : lexer cs
-}
lexer s@( c : cs )
	| isSpace c	= lexer cs
	| isDigit c	= let ( ret, rest ) = span isDigit s in
				Integer ( read ret ) : lexer rest
	| c `elem` "+-*/" = Op ( Other [ c ] ) : lexer cs
lexer _			= [ TokError "Error: lexer has not recognized" ]

type Parser = GenParser Token ()

token :: ( Token -> Maybe a ) -> Parser a
token test = P.token show ( const $ initialPos "" ) test

tokenTest :: Token -> Token -> Maybe ()
tokenTest tok0 tok1
	| tok1 == tok0	= Just ()
	| otherwise	= Nothing

tokenOp :: Token -> Maybe Op
tokenOp ( Op op )	= Just op
tokenOp _		= Nothing

tokenOpPM :: Token -> Maybe Op
tokenOpPM ( Op op@Plus )	= Just op
tokenOpPM ( Op op@Minus )	= Just op
tokenOpPM _			= Nothing

tokenOpMD :: Token -> Maybe Op
tokenOpMD ( Op op@Multi )	= Just op
tokenOpMD ( Op op@Div )		= Just op
tokenOpMD _			= Nothing

tokenNum :: Token -> Maybe Expr
tokenNum ( Integer i )	= Just $ Num i
tokenNum _		= Nothing

parseExpr :: [ Token ] -> Either ParseError Expr
parseExpr = parse parser ""

parser :: Parser Expr
parser = parserExpr >>= \ret -> eof >> return ret

parserExpr :: Parser Expr
parserExpr = buildExpressionParser table' $ token tokenNum

parserOne :: Parser Expr
parserOne = do
	n <- token tokenNum
	f <- parserOne'
	return $ f n

parserOne' :: Parser ( Expr -> Expr )
parserOne' = option id $ do
	op <- token tokenOp
	n2 <- token tokenNum
	f <- parserOne'
	return $ \n1 -> f $ Expr op n1 n2

table :: [ [ Operator Token () Expr ] ]
table = [
	[ Infix ( do { op <- token tokenOpMD; return $ Expr op } ) AssocLeft ],
	[ Infix ( do { op <- token tokenOpPM; return $ Expr op } ) AssocLeft ]
 ]

mkTable :: [ ( String, Int, Assoc ) ] -> OperatorTable Token () Expr
mkTable = foldr ( uncurry3 insertToTable ) $ replicate 10 [ ]

buildExprParser :: [ ( String, Int, Assoc ) ] -> Parser Expr -> Parser Expr
buildExprParser tbl atom = buildExprParser tbl atom

table' :: OperatorTable Token () Expr
table' = mkTable [
	( "+", 6, AssocLeft ),
	( "-", 6, AssocLeft ),
	( "*", 7, AssocLeft ),
	( "/", 7, AssocLeft )
 ]

uncurry3 :: ( a -> b -> c -> d ) -> ( a, b, c ) -> d
uncurry3 f ( x, y, z ) = f x y z

insertToTable :: String -> Int -> Assoc -> OperatorTable Token () Expr ->
	OperatorTable Token () Expr
insertToTable op power assoc tbl =
	take pos tbl ++ [ fx : tbl !! pos ] ++ drop ( pos + 1 ) tbl
	where
	pos = 9 - power
	fx = Infix ( do
		token $ eqM $ Op $ Other op
		return $ Expr $ Other op ) assoc

eqM :: Eq a => a -> a -> Maybe ()
eqM x y	| x == y	= Just ()
	| otherwise	= Nothing

eval :: Expr -> Integer
eval ( Num i ) = i
eval ( Expr Plus e1 e2 ) = eval e1 + eval e2
eval ( Expr Minus e1 e2 ) = eval e1 - eval e2
eval ( Expr Multi e1 e2 ) =  eval e1 * eval e2
eval ( Expr Div e1 e2 ) = eval e1 `div` eval e2
eval ( Expr ( Other "+" ) e1 e2 ) = eval ( Expr Plus e1 e2 )
eval ( Expr ( Other "-" ) e1 e2 ) = eval ( Expr Minus e1 e2 )
eval ( Expr ( Other "*" ) e1 e2 ) = eval ( Expr Multi e1 e2 )
eval ( Expr ( Other "/" ) e1 e2 ) = eval ( Expr Div e1 e2 )
