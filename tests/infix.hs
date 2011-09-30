{-# LANGUAGE PatternGuards #-}

module Main where

import Prelude hiding (Either(..))
import System.Environment (getArgs)
import Data.Char (isSpace, isDigit)
import Data.List (isPrefixOf)

import Parse (Parse, spot, token, eof, (>*>), alt, build, list1)
import Tools (mapFilter)

main :: IO ()
main = do
	cnt	<- getArgs >>= readFile . head
	let	parsed	= parse $ lexer cnt
		expr	= mapFilter getValue parsed
		fix	= mapFilter getFixity parsed
	mapM_ ( print . fixInfix fix) expr
	where
	getValue (Expression v)		= Just v
	getValue _			= Nothing
	getFixity (FixityDecl f)	= Just f
	getFixity _			= Nothing

data Parsed	= FixityDecl (String, Fixity) | Expression Infix deriving Show

data Infix	= Op String Value Infix | Atom Value deriving Show
data Value	= Infix Infix | OpV String Value Value | Int Int

data Assoc	= Left | Right | None deriving Show
data Fixity	= Fix Assoc Int deriving Show

instance Show Value where
	show (Int n)		= show n
	show (OpV op v1 v2)	=
		"(" ++ show v1 ++ " " ++ op ++ " " ++ show v2 ++ ")"
	show (Infix i)		= showInfix i
		where
		showInfix (Atom v)	= show v
		showInfix (Op op v1 v2)	= show v1 ++ " " ++ op ++ " " ++ showInfix v2

fixInfix :: [(String, Fixity)] -> Infix -> Value
fixInfix fix = fixValue . fixToV
	where
	fixToV o@(Op _ _ _)	= fixToV $ fixity o
	fixToV (Atom v)		= v
	fixity (Atom v)		= Atom v
	fixity (Op op1 v1 (Op op2 v2 i))	= case compFixity op1 op2 of
		LT	-> Op op1 v1 $ fixity $ Op op2 v2 i
		_	-> Op op2 (OpV op1 v1 v2) i
	fixity (Op op1 v i)			= Atom $ OpV op1 v $ Infix i
	fixValue (Infix i)		= fixValue $ fixToV i
	fixValue (OpV op v1 v2)	= OpV op (fixValue v1) (fixValue v2)
	fixValue v			= v
	compFixity op1 op2 = case (lookup op1 fix, lookup op2 fix) of
		( Just f1, Just f2 )	-> compInfix f1 f2
		( Just f1, Nothing )	-> compInfix f1 (Fix Left 9)
		( Nothing, Just f2 )	-> compInfix (Fix Left 9) f2
		( Nothing, Nothing )	-> compInfix (Fix Left 9) (Fix Left 9)
	compInfix (Fix assc1 prec1) (Fix assc2 prec2)
		| prec1 > prec2				= GT
		| prec1 < prec2				= LT
		| Left <- assc1, Left <- assc2		= GT
		| Right <- assc1, Right <- assc2	= LT
		| otherwise				= error "bad associativity"

parse :: [Token] -> [Parsed]
parse = fst . head . parser

parser :: Parse Token [Parsed]
parser = list1 parserOne >*> eof `build` fst

parserOne, parserInfix :: Parse Token Parsed
parserOne = parserInfix `alt` parserExp `build` Expression

parserInfix = token Infixl >*> spot isNum >*> spot isSym
	`build` (\(_, (Num n, Sym s)) -> FixityDecl (s, Fix Left n))
	`alt`
	token Infixr >*> spot isNum >*> spot isSym
	`build` (\(_, (Num n, Sym s)) -> FixityDecl (s, Fix Right n))
	`alt`
	token Infixn >*> spot isNum >*> spot isSym
	`build` (\(_, (Num n, Sym s)) -> FixityDecl (s, Fix None n))

parserExp :: Parse Token Infix
parserExp =
	parserAtom `build` Atom
	`alt`
	parserAtom >*> spot isSym >*> parserExp `build`
		\(e1, (Sym op, e)) -> Op op e1 e

parserAtom :: Parse Token Value
parserAtom =
	spot isNum `build` (\(Num n) -> Int n)
	`alt`
	token OpenParen >*> parserExp >*> token CloseParen `build` Infix . fst . snd

data Token	= Infixl | Infixr | Infixn | Num Int | Comma | Semi | Sym String
		| OpenParen | CloseParen
	deriving (Eq, Show)

isNum :: Token -> Bool
isNum (Num _)	= True
isNum _		= False

isSym :: Token -> Bool
isSym (Sym _)	= True
isSym _		= False

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
	| otherwise			= error "lexer error"

getNum :: String -> Maybe (Token, String)
getNum ca@(c : _)
	| isDigit c	= Just (Num $ read $ takeWhile isDigit ca,
							dropWhile isDigit ca )
getNum _		= Nothing

getSym :: String -> Maybe (Token, String)
getSym ca@(c : _)
	| isSymbol c	= Just (Sym $ takeWhile isSymbol ca, dropWhile isSymbol ca)
	where isSymbol	= (`elem` "+-*/!.^:=<>&|$")
getSym _		= Nothing

getInfixes :: String -> Maybe (Token, String)
getInfixes ca
	| "infixl" `isPrefixOf` ca	= Just (Infixl, drop 6 ca)
	| "infixr" `isPrefixOf` ca	= Just (Infixr, drop 6 ca)
	| "infix" `isPrefixOf` ca	= Just (Infixn, drop 5 ca)
	| otherwise			= Nothing
