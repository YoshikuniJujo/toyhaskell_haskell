module Main where

import System.Environment
import Data.Tree
import Data.Char

data Op		= Plus | Minus | Times | Div deriving Show
data Elem	= Op Op | Int Int deriving Show
type Expr	= Tree Elem

lexer :: String -> [ Elem ]
lexer ""		= [ ]
lexer ( ' ' : cs )	= lexer cs
lexer ( '+' : cs )	= Op Plus : lexer cs
lexer ( '-' : cs )	= Op Minus : lexer cs
lexer ( '*' : cs )	= Op Times : lexer cs
lexer ( '/' : cs )	= Op Div : lexer cs
lexer ca@( c : _ )
	| isDigit c	= let ( ret, rest ) = span isDigit ca in
				Int ( read ret ) : lexer rest
lexer _			= error "lex error"

parser :: [ Elem ] -> Expr
parser [ e@( Int _ ) ]				= Node e [ ]
parser ( e1@( Int _ ) : e2@( Op _ ) : rest )	= Node e2 [ Node e1 [ ], parser rest ]

eval :: Expr -> Int
eval ( Node ( Int i ) [ ] )		= i
eval ( Node ( Op Plus ) [ e1, e2 ] )	= eval e1 + eval e2
eval ( Node ( Op Minus ) [ e1, e2 ] )	= eval e1 - eval e2
eval ( Node ( Op Times ) [ e1, e2 ] )	= eval e1 * eval e2
eval ( Node ( Op Div ) [ e1, e2 ] )	= eval e1 `div` eval e2

main :: IO ()
main = do
	[ expr ] <- getArgs
	print $ eval $ parser $ lexer expr
