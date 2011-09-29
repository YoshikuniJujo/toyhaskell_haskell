{-# LANGUAGE PatternGuards #-}

module Main where

import Prelude hiding (Either(..))
import Parse
import System.Environment
import Data.Char
import Data.List
import Data.Function
import Data.Maybe

main :: IO ()
main = do
	[fn]	<- getArgs
	cnt	<- readFile fn
	let	parsed	= parse $ lexer cnt
		exp	= mapFilter getValue parsed
	putStr cnt
	print $ mapFilter getFixity parsed
	print $ mapFilter getValue parsed
	mapM_ putStrLn $ map showValue exp
	mapM_ putStrLn $ map showValue $ map fixToLeftV exp

data Infix	= Op String Infix Infix | V Value deriving Show
data Value	= Infix Infix | OpV String Value Value | Int Int deriving Show
data Assoc	= Left | Right | None deriving Show
data Fixity	= Fix Assoc Int deriving Show
data Parsed	= Fixity (String, Fixity) | Value Value deriving Show

fixities :: [(String, Int)]
fixities = [("+", 6), ("-", 6), ("*", 7), ("/", 7)]

compFixity :: String -> String -> Ordering
compFixity = on compare (fromJust . flip lookup fixities)

fixToLeft :: Infix -> Value
fixToLeft (V v)				= fixToLeftV v
fixToLeft (Op op1 i1 (Op op2 i2 i3))	= case compFixity op1 op2 of
	LT	-> OpV op1 (fixToLeft i1) $ fixToLeft $ Op op2 i2 i3
	_	-> fixToLeft $ Op op2 (V $ OpV op1 (fixToLeft i1) $ fixToLeft i2) i3
fixToLeft (Op op i1 i2)			= OpV op (fixToLeft i1) (fixToLeft i2)

fixToLeftV :: Value -> Value
fixToLeftV (Infix i)		= fixToLeft i
fixToLeftV o@(OpV _ _ _ )	= o
fixToLeftV n@(Int _)		= n

showValue :: Value -> String
showValue (Int n)		= show n
showValue (Infix i)		= showInfix i
showValue (OpV op v1 v2)	=
	"(" ++ showValue v1 ++ " " ++ op ++ " " ++ showValue v2 ++ ")"

showInfix :: Infix -> String
showInfix (V v)		= showValue v
showInfix (Op op v1 v2)	=
	"(" ++ showInfix v1 ++ " " ++ op ++ " " ++ showInfix v2 ++ ")"

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f []		= []
mapFilter f (x : xs)
	| Just y <- f x	= y : mapFilter f xs
	| otherwise	= mapFilter f xs

getValue :: Parsed -> Maybe Value
getValue (Value v)	= Just v
getValue _		= Nothing

getFixity :: Parsed -> Maybe (String, Fixity)
getFixity (Fixity f)	= Just f
getFixity _		= Nothing

parse :: [Token] -> [Parsed]
parse = fst . head . parser

parser :: Parse Token [Parsed]
parser = list1 parserOne >*> eof `build` fst

parserOne, parserInfix :: Parse Token Parsed
parserOne = parserInfix `alt` parserExp `build` Value . Infix

parserInfix = token Infixl >*> spot isNum >*> spot isSym
	`build` \(_, (Num n, Sym s)) -> Fixity $ (s, Fix Left n)

parserExp, parserAtom :: Parse Token Infix
parserExp =
	parserAtom
	`alt`
	parserAtom >*> spot isSym >*> parserExp `build`
		\(e1, (Sym op, e)) -> Op op e1 e

parserAtom =
	spot isNum `build` (\(Num n) -> V (Int n))
	`alt`
	token OpenParen >*> parserExp >*> token CloseParen `build` fst . snd

data Token	= Infixl | Infixr | {- Infix | -} Num Int | Comma | Semi | Sym String
		| OpenParen | CloseParen
	deriving (Eq, Show)

isNum :: Token -> Bool
isNum (Num _)	= True
isNum _		= False

isSym :: Token -> Bool
isSym (Sym _)	= True
isSym _		= False

isSymb :: Char -> Bool
isSymb = (`elem` "+-*/")

lexer :: String -> [Token]
lexer ""				= []
lexer (';' : cs)			= Semi : lexer cs
lexer (',' : cs)			= Comma : lexer cs
lexer ('(' : cs)			= OpenParen : lexer cs
lexer (')' : cs)			= CloseParen : lexer cs
lexer ca@(c : cs)
	| isSpace c			= lexer cs
	| Just (t, r) <- getNum ca	= t : lexer r
	| Just (t, r) <- getSym ca	= t : lexer r
	| Just (t, r) <- getInfixes ca	= t : lexer r

getNum :: String -> Maybe (Token, String)
getNum ca@(c : _)
	| isDigit c	= Just (Num $ read $ takeWhile isDigit ca,
							dropWhile isDigit ca )
	| otherwise	= Nothing

getSym :: String -> Maybe (Token, String)
getSym ca@(c : _)
	| isSymb c	= Just (Sym $ takeWhile isSymb ca, dropWhile isSymb ca)
	| otherwise	= Nothing

getInfixes :: String -> Maybe (Token, String)
getInfixes ca
	| "infixl" `isPrefixOf` ca	= Just (Infixl, drop 6 ca)
	| otherwise			= Nothing
