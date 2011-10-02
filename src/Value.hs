{-# LANGUAGE TupleSections #-}

module Value (
	Infix(..),
	Value(..),
	Pattern(..),
	patVars,
	match,

	Env,
	Var(V),
	initialize,
	setVars,
	setPat,
	setPats,
	getVal,
	getVars,
	mapEnv
) where

import Prelude hiding (showList)

import Env (Var(V), getVars, mapEnv)
import qualified Env (Env, initialize, setVars, setPat, setPats, getVal)
import Control.Monad (liftM, zipWithM)

--------------------------------------------------------------------------------

data Infix	= Op String Value Infix
		| Value Value
	deriving Show

data Value =
	Nil				|
	Empty				|
	Integer Integer			|
	Char Char			|
	Fun (Value -> Value)		|
	IOAction (IO Value)		|
	Var String Int			|
	Con String [Value]		|
	App Value Value			|
	Lambda [Pattern] Value		|
	Closure Env [Pattern] Value	|
	Case Value [(Pattern, Value)]	|
	Letin [(Pattern, Value)] Value	|
	Module [(Pattern, Value)]	|
	Let [(Pattern, Value)]		|
	Infix Infix			|
	Err String

data Pattern =
	PatNil				|
	PatEmpty			|
	PatInteger Integer		|
	PatVar String Int		|
	PatUScore			|
	PatCon String [Pattern]
	deriving (Eq, Show)

instance Show Value where
	show Nil		= "()"
	show Empty		= "[]"
	show (Integer n)	= show n
	show (Char c)		= show c
	show (Fun _)		= "<function>"
	show (IOAction _)	= "<IO>"
	show (Var i 0)		= i
	show (Var i n)		= i ++ "~" ++ show n
	show v@(Con ":" [Char _, _])
				= "\"" ++ showStr v ++ "\""
	show v@(Con ":" _)	= "[" ++ showList v ++ "]"
	show (Con n [])		= n
	show (Con n mems)	= "(" ++ n ++ " " ++ unwordsMap show mems ++ ")"
	show (App f a)		= "(" ++ show f ++ " " ++ show a ++ ")"
	show (Lambda ps ex)	= showLambda ps ex
	show (Closure _ _ _)	= "<closure>"
	show (Case key alts)	= showCase key alts
	show (Letin defs ex)	= "let " ++ showDefs defs ++ " in " ++ show ex
	show (Module defs)	= "module " ++ showDefs defs
	show (Let defs)		= "let " ++  showDefs defs
	show (Infix _)		= "<infix>"
	show (Err msg)		= "Error: " ++ msg

showStr :: Value -> String
showStr Empty				= ""
showStr (Con ":" [Char '\\', s])	= '\\' : '\\' : showStr s
showStr (Con ":" [Char '\n', s])	= '\\' : 'n' : showStr s
showStr (Con ":" [Char c, s])		= c : showStr s
showStr _				= "Error: bad String"

showList :: Value -> String
showList (Con ":" [v, Empty])	= show v
showList (Con ":" [v, lst])	= show v ++ "," ++ showList lst
showList _			= "Error: bad List"

showLambda :: [Pattern] -> Value -> String
showLambda ps ex = "(\\" ++ unwordsMap showPat ps ++ " -> " ++ show ex ++ ")"

showCase :: Value -> [(Pattern, Value)] -> String
showCase key alts = "case " ++ show key ++ " of { " ++
	unwordsMap (\(t, ex) -> showPat t ++ " -> " ++ show ex ++ "; ") alts
	++ " }"

showDefs :: [(Pattern, Value)] -> String
showDefs = unwordsMap (\(p, v) -> showPat p ++ " = " ++ show v ++ ";")

showPat :: Pattern -> String
showPat (PatVar var 0)	= var
showPat (PatVar var n)	= var ++ "~" ++ show n
showPat p		= show p

unwordsMap :: (a -> String) -> [a] -> String
unwordsMap = (.) unwords . map

match :: Value -> Pattern -> Maybe [(Var, Value)]
match val (PatVar var n)	= Just [(V var n, val)]
match _ PatUScore		= Just []
match (Integer i1) (PatInteger i0)
	| i1 == i0		= Just []
	| otherwise		= Nothing
match (Con name1 vals) (PatCon name0 pats)
	| name1 == name0	= liftM concat $ zipWithM match vals pats
	| otherwise		= Nothing
match Empty PatEmpty		= Just []
match _ _			= Nothing

patVars :: Pattern -> [String]
patVars = map (\(V x _) -> x) . patToVars

patToVars :: Pattern -> [Var]
patToVars (PatCon _ pats)	= patToVars `concatMap` pats
patToVars (PatVar var n)	= [V var n]
patToVars _			= []

--------------------------------------------------------------------------------

type Env = Env.Env Pattern Value

initialize :: [(String, Value)] -> Env
initialize = Env.initialize (flip PatVar 0)

setVars :: Env -> [(Var, Value)] -> Env
setVars = Env.setVars (\(V x n) -> PatVar x n)

setPat :: Env -> Pattern -> Value -> Env
setPat = Env.setPat patToVars

setPats :: Env -> [(Pattern, Value)] -> Env
setPats = Env.setPats patToVars

getVal :: (Value -> Value) -> Env -> Var -> Maybe Value
getVal = Env.getVal match
