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
	parsed <- (parse . lexer) `fmap` (getArgs >>= readFile . head)
	let	expr	= mapFilter getValue parsed
		fix	= mapFilter getFixity parsed
	mapM_ (print . fixInfix fix) expr
	where
	getValue (Expression v)		= Just v
	getValue _			= Nothing
	getFixity (FixityDecl f)	= Just f
	getFixity _			= Nothing

data Parsed	= FixityDecl (String, Fixity) | Expression Infix deriving Show

data Infix	= Op String Value Infix | Atom Value deriving Show
data Value	= Infix Infix | OpV String Value Value | Int Int

data Assoc	= Left | Right | None deriving (Eq, Show)
data Fixity	= Fix Assoc Int deriving Show

instance Show Value where
	show (Int n)		= show n
	show (OpV op v1 v2)	=
		"(" ++ show v1 ++ " " ++ op ++ " " ++ show v2 ++ ")"
	show (Infix i)		= showInfix i
		where
		showInfix (Atom v)	= show v
		showInfix (Op op v1 v2)	=
			show v1 ++ " " ++ op ++ " " ++ showInfix v2

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

--------------------------------------------------------------------------------

parse :: [Token] -> [Parsed]
parse = fst . head . parser

parser :: Parse Token [Parsed]
parser = list1 parserOne >*> eof `build` fst

parserOne, parserInfix :: Parse Token Parsed
parserOne = parserInfix `alt` parserExp `build` Expression

parserInfix = spot isInfix >*> spot isNum >*> spot isSym
	`build` \(TInfix assc, (Num n, Sym s)) -> FixityDecl (s, Fix assc n)

parserExp :: Parse Token Infix
parserExp = parserAtom `build` Atom `alt`
	parserAtom >*> spot isSym >*> parserExp `build`
		\(e1, (Sym op, e)) -> Op op e1 e

parserAtom :: Parse Token Value
parserAtom =
	spot isNum `build` (\(Num n) -> Int n)
	`alt`
	token OP >*> parserExp >*> token CP `build` Infix . fst . snd

--------------------------------------------------------------------------------

data Token = TInfix Assoc | Num Int | Comma | Semi | Sym String | OP | CP
	deriving (Eq, Show)

isNum :: Token -> Bool
isNum (Num _)	= True
isNum _		= False

isSym :: Token -> Bool
isSym (Sym _)	= True
isSym _		= False

isInfix :: Token -> Bool
isInfix (TInfix _)	= True
isInfix _		= False

lexer :: String -> [Token]
lexer ""				= []
lexer (';' : cs)			= Semi : lexer cs
lexer (',' : cs)			= Comma : lexer cs
lexer ('(' : cs)			= OP : lexer cs
lexer (')' : cs)			= CP : lexer cs
lexer ca@(c : cs)
	| isSpace c			= lexer cs
	| Just (t, r) <- num		= t : lexer r
	| Just (t, r) <- sym		= t : lexer r
	| Just (t, r) <- infixes	= t : lexer r
	| otherwise			= error "lexer error"
	where
	isS			= (`elem` "+-*/!.^:=<>&|$")
	num	| isDigit c	= let (t, r) = span isDigit ca in
							Just (Num $ read t, r)
		| otherwise	= Nothing
	sym	| isS c		= let (t, r) = span isS ca in Just (Sym t, r)
		| otherwise	= Nothing
	infixes	| "infixl" `isPrefixOf` ca	= Just (TInfix Left, drop 6 ca)
		| "infixr" `isPrefixOf` ca	= Just (TInfix Right, drop 6 ca)
		| "infix" `isPrefixOf` ca	= Just (TInfix None, drop 5 ca)
		| otherwise			= Nothing
