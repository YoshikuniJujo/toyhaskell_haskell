{-# LANGUAGE PatternGuards #-}

module Main where

import Prelude hiding (Either(..))
import Parse (Parse, spot, token, eof, (>*>), alt, build, list1)
import System.Environment (getArgs)
import Data.Char (isSpace, isDigit)
import Data.List (isPrefixOf)
import Data.Maybe (fromJust)
import Data.Function (on)

main :: IO ()
main = do
	cnt	<- getArgs >>= readFile . head
	let	parsed	= parse $ lexer cnt
		exp	= mapFilter getValue parsed
		fix	= mapFilter getFixity parsed
	mapM_ (putStrLn . showValue . fixValue fix) exp

data Parsed	= FixityDecl (String, Fixity) | Expression Value deriving Show

data Infix	= Op String Value Infix | Atom Value deriving Show
data Value	= Infix Infix | OpV String Value Value | Int Int deriving Show

data Assoc	= Left | Right | None deriving Show
data Fixity	= Fix Assoc Int deriving Show

compFixity :: [(String, Fixity)] -> String -> String -> Ordering
compFixity fix op1 op2 = case (lookup op1 fix, lookup op2 fix) of
	( Just f1, Just f2 )	-> compInfix f1 f2
	( Just f1, Nothing )	-> compInfix f1 (Fix Left 9)
	( Nothing, Just f2 )	-> compInfix (Fix Left 9) f2
	( Nothing, Nothing )	-> compInfix (Fix Left 9) (Fix Left 9)

compInfix :: Fixity -> Fixity -> Ordering
compInfix (Fix assc1 prec1) (Fix assc2 prec2)
	| prec1 > prec2				= GT
	| prec1 < prec2				= LT
	| Left <- assc1, Left <- assc2		= GT
	| Right <- assc1, Right <- assc2	= LT
	| otherwise				= error "bad associativity"

fixToV :: [(String, Fixity)] -> Infix -> Value
fixToV fix o@(Op _ _ _)	= fixToV fix $ fixity fix o
fixToV fix (Atom v)	= v

fixValue :: [(String, Fixity)] -> Value -> Value
fixValue fix (Infix i)		= fixValue fix $ fixToV fix i
fixValue fix (OpV op v1 v2)	= OpV op (fixValue fix v1) (fixValue fix v2)
fixValue _ v			= v

fixity :: [(String, Fixity)] -> Infix -> Infix
fixity _ (Atom v)			= Atom v
fixity fix (Op op1 v1 (Op op2 v2 i))	= case compFixity fix op1 op2 of
	LT	-> Op op1 v1 $ fixity fix $ Op op2 v2 i
	_	-> Op op2 (OpV op1 v1 v2) i
fixity _ (Op op1 v i)			= Atom $ OpV op1 v $ Infix i

fixityV :: [(String, Fixity)] -> Value -> Infix
fixityV fix (Infix i) = fixity fix i

showValue :: Value -> String
showValue (Int n)		= show n
showValue (Infix i)		= showInfix i
showValue (OpV op v1 v2)	=
	"(" ++ showValue v1 ++ " " ++ op ++ " " ++ showValue v2 ++ ")"

showInfix :: Infix -> String
showInfix (Atom v)	= showValue v
showInfix (Op op v1 v2)	= showValue v1 ++ " " ++ op ++ " " ++ showInfix v2

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f []		= []
mapFilter f (x : xs)
	| Just y <- f x	= y : mapFilter f xs
	| otherwise	= mapFilter f xs

getValue :: Parsed -> Maybe Value
getValue (Expression v)	= Just v
getValue _		= Nothing

getFixity :: Parsed -> Maybe (String, Fixity)
getFixity (FixityDecl f)	= Just f
getFixity _			= Nothing

parse :: [Token] -> [Parsed]
parse = fst . head . parser

parser :: Parse Token [Parsed]
parser = list1 parserOne >*> eof `build` fst

parserOne, parserInfix :: Parse Token Parsed
parserOne = parserInfix `alt` parserExp `build` Expression . Infix

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

isSymb :: Char -> Bool
isSymb = (`elem` "+-*/!.^:=<>&|$")

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
	| "infixr" `isPrefixOf` ca	= Just (Infixr, drop 6 ca)
	| "infix" `isPrefixOf` ca	= Just (Infixn, drop 5 ca)
	| otherwise			= Nothing
