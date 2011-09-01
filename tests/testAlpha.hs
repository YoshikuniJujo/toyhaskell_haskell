module Main where

import Data.List
import Data.Char
import Control.Arrow
import Interact

data Exp
	= Int Int
	| Var String
	| Apply Exp Exp
	| Lambda [ String ] Exp
	| Let [ ( String, Exp ) ] Exp
	deriving Show

showExp :: Exp -> String
showExp ( Int int )		= show int
showExp ( Var var )		= var
showExp ( Apply fun arg )	=
	"(" ++ showExp fun ++ " " ++ showExp arg ++ ")"
showExp ( Lambda vars body )	=
	"\\" ++ unwords vars ++ " -> " ++ showExp body
showExp ( Let ps body )	=
	"let " ++ unwords ( map ( \( var, val ) ->
		var ++ " = " ++	showExp val ++ ";" ) ps ) ++
	" in " ++ showExp body

main :: IO ()
main = do
	putStrLn $ showExp $ Lambda [ "x" ] ( Var "x" )
	print $ head $ parse $ lexer "\\x y -> ( f x ) y"
	runLoop "alpha" () $ \() input -> do
		case parse $ lexer input of
			[ ]	-> putStrLn "parse error"
			ret : _	-> putStrLn $ showExp $ alpha [ ] $ fst ret

type Parser a = [ Token ] -> [ ( a, [ Token ] ) ]

next :: Parser a -> Parser b -> Parser ( a, b )
( p1 `next` p2 ) ts = [ ( ( r1, r2 ), rest2 ) |
	( r1, rest1 ) <- p1 ts, ( r2, rest2 ) <- p2 rest1 ]

build :: Parser a -> ( a -> b ) -> Parser b
( p `build` f ) ts = map ( first f ) $ p ts

alt :: Parser a -> Parser a -> Parser a
( p1 `alt` p2 ) ts = p1 ts ++ p2 ts

spot :: ( Token -> Bool ) -> Parser Token
spot p ( t : rest )
	| p t		= [ ( t, rest ) ]
spot _	_		= [ ]

token :: Token -> Parser Token
token = spot . ( == )

succeed :: a -> Parser a
succeed x ts = [ ( x, ts ) ]

eof :: Parser ()
eof [ ] = [ ( (), [ ] ) ]
eof _	= [ ]

list1 :: Parser a -> Parser [ a ]
list1 p = ( p `build` ( : [] ) ) `alt` ( ( p `next` list1 p ) `build` \( r, rs ) -> r : rs )

parse :: Parser Exp
parse = parseExp `next` eof `build` fst

parseExp :: Parser Exp
parseExp = parseApply `alt` parseLambda `alt` parseLetin

parseAtom :: Parser Exp
parseAtom = parseInt `alt` parseVar `alt` parseParens

parseParens :: Parser Exp
parseParens = token TokOpenParen `next` parseExp `next` token TokCloseParen
	`build` ( \( ( _, e ), _ ) -> e )

parseVar :: Parser Exp
parseVar = spot isTokVar `build` ( \( TokVar var ) -> Var var )

parseInt :: Parser Exp
parseInt = spot isTokInt `build` ( \( TokInt int ) -> Int int )

parseVarStr :: Parser String
parseVarStr = spot isTokVar `build` ( \( TokVar var ) -> var )

parseApply :: Parser Exp
parseApply = parseAtom `next` parseApply' `build` \( a, b ) -> b a

parseApply' :: Parser ( Exp -> Exp )
parseApply' = succeed id `alt`
	( parseAtom `next` parseApply' `build` \( a, b ) ->
	\v -> b ( Apply v a ) )

parseLambda :: Parser Exp
parseLambda = token TokLambda `next` list1 parseVarStr `next`
	token TokArrow `next` parseExp `build` \( ( ( _, vars ), _ ), exp ) -> Lambda vars exp

parseEq :: Parser ( String, Exp )
parseEq = parseVarStr `next` token TokEq `next` parseExp `next` token TokSemi `build`
	\( ( ( var, _ ), exp ), _ ) -> ( var, exp )

parseLetin :: Parser Exp
parseLetin = token TokLet `next` list1 parseEq `next` token TokIn `next` parseExp
	`build` \( ( ( _, eqs ), _ ), exp ) -> Let eqs exp

data Token
	= TokInt Int
	| TokVar String
	| TokLet
	| TokEq
	| TokIn
	| TokLambda
	| TokArrow
	| TokOpenParen
	| TokCloseParen
	| TokSemi
	deriving ( Eq, Show )

isTokVar, isTokInt :: Token -> Bool
isTokVar ( TokVar _ )	= True
isTokVar _		= False
isTokInt ( TokInt _ )	= True
isTokInt _		= False

lexer :: String -> [ Token ]
lexer ""			= [ ]
lexer ( ' ' : cs )		= lexer cs
lexer ( '\\' : cs )		= TokLambda : lexer cs
lexer ( '=' : cs )		= TokEq : lexer cs
lexer ( '(' : cs )		= TokOpenParen : lexer cs
lexer ( ')' : cs )		= TokCloseParen : lexer cs
lexer ( ';' : cs )		= TokSemi : lexer cs
lexer ( '-' : '>' : cs )	= TokArrow : lexer cs
lexer ( 'l' : 'e' : 't' : cs )	= TokLet : lexer cs
lexer ( 'i' : 'n' : cs )	= TokIn : lexer cs
lexer str@( c : _ )
	| isLower c		= let ( var, rest ) = span isLower str in
					TokVar var : lexer rest
	| isDigit c		= let ( num, rest ) = span isDigit str in
					TokInt ( read num ) : lexer rest
lexer _				= error "bad"

-- alpha :: [ ( String, Int ) ] -> Exp -> Exp
alpha used ( Lambda vars body ) = Lambda nvars $ alpha nused $ putTildas usedV body
	where
	nused = used ++ ( vars \\ used )
	usedV = vars `intersect` used
	nvars = ( vars \\ used ) ++ map ( ++ "~" ) usedV
{-
alpha used ( Let ps body ) = Let nps $ alpha nused $ putTildas usedV body
	where
	nused = 
-}
alpha _ e = e

putTildas :: [ String ] -> Exp -> Exp
putTildas = flip ( foldr putTilda )

putTilda :: String -> Exp -> Exp
putTilda var = changeVar var $ var ++ "~"

changeVar :: String -> String -> Exp -> Exp
changeVar p a ( Lambda vars body )	= Lambda vars $ changeVar p a body
changeVar p a ( Let ps body )		= Let ps $ changeVar p a body
changeVar p a ( Apply f arg )		= Apply ( changeVar p a f )
						( changeVar p a arg )
changeVar p a ( Var v )
	| p == v			= Var a
changeVar _ _ e				= e
