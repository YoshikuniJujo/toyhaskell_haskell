module Main where

import Text.ParserCombinators.Parsec hiding ( Parser, token )
import qualified Text.ParserCombinators.Parsec as P
import Text.ParserCombinators.Parsec.Pos
import Text.ParserCombinators.Parsec.Expr
import Data.Char
import Interact

import BuildExpression

main :: IO ()
main = runLoop "expr" () $ \() input ->
	putStrLn $ either show ( show . eval ) $ parseExpr $ lexer input

type Parser = GenParser Token ()

token :: Show a => ( a -> Maybe b ) -> GenParser a st b
token = P.token show ( const $ initialPos "" )

tokenTest :: Token -> Token -> Maybe ()
tokenTest tok0 tok1
	| tok1 == tok0	= Just ()
	| otherwise	= Nothing

tokenNum :: Token -> Maybe Expr
tokenNum ( Integer i )	= Just $ Num i
tokenNum _		= Nothing

parseExpr :: [ Token ] -> Either ParseError Expr
parseExpr = parse parser ""

parser :: Parser Expr
parser = parserExpr >>= \ret -> eof >> return ret

parserExpr :: Parser Expr
parserExpr = buildExprParser show ( const $ initialPos "" )table $ token tokenNum

table :: OpTable Token () Expr
table = [
	( Op $ Other "+", Expr $ Other "+", 6, AssocLeft ),
	( Op $ Other "-", Expr $ Other "-", 6, AssocLeft ),
	( Op $ Other "*", Expr $ Other "*", 7, AssocLeft ),
	( Op $ Other "/", Expr $ Other "/", 7, AssocLeft )
 ]

--------------------------------------------------------------------------------

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
eval _ = error "yet"
