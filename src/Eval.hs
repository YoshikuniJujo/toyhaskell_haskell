module Eval ( toyEval ) where

import Value ( Value(
	Identifier, Apply, Function, Lambda, Closure, Case, Letin, Let,	Module,
	Error ),
	Pattern, match, Env, setVars, setPat, setPats, getVal, getPatVars )
import Data.Maybe ( fromMaybe )

--------------------------------------------------------------------------------

toyEval :: Env -> Value -> Value
toyEval env v = case noVars env v of
	[ ]	-> eval env v
	vars	-> Error $ unlines $ map errMsg vars
	where errMsg var = "\tNot in scope: `" ++ var ++ "'"

getVars :: Pattern -> [ String ]
getVars = getPatVars

getV :: Env -> String -> Maybe Value
getV env var = getVal ( eval env ) var env

filterVars :: [ Pattern ] -> [ String ] -> [ String ]
filterVars pats vars = ( `notElem` getVars `concatMap` pats ) `filter` vars

noVars :: Env -> Value -> [ String ]
noVars env ( Identifier i _ )	= maybe [ i ] ( noVars env ) $ getV env i
noVars env ( Apply f a )	= noVars env f ++ noVars env a
noVars env ( Lambda vs body )	= vs `filterVars` noVars env body
noVars env ( Case v sel )	= noVars env v ++ noVarsC env `concatMap` sel
noVars env ( Letin defs body )	= map fst defs `filterVars`
	( noVars env body ++ ( noVars env . snd ) `concatMap` defs )
noVars env ( Let defs )		= map fst defs `filterVars`
	( ( noVars env . snd ) `concatMap` defs )
noVars env ( Module defs )	= noVars env ( Let defs )
noVars _ _			= [ ]

noVarsC :: Env -> ( Pattern, Value ) -> [ String ]
noVarsC env ( pat, val ) = ( `notElem` getVars pat ) `filter` noVars env val

eval :: Env -> Value -> Value
eval env ( Identifier i _ )	= eval env $ fromMaybe ( noVar i ) $ getV env i
eval env ( Apply f a )		= case eval env f of
	Function fun		-> fun $ eval env a
	Closure ce [ p ] b	-> eval ( setPat p ( eval env a ) ce ) b
	Closure ce ( p : ps ) b	-> Closure ( setPat p ( eval env a ) ce ) ps b
	e@( Error _ )		-> e
	_			-> notFunction f
eval env ( Lambda pats body )	= Closure env pats body
eval env ( Case val bodys )	= evalC env ( eval env val ) bodys
eval env ( Letin defs body )	= eval ( setPats defs env ) body
eval _ v			= v

evalC :: Env -> Value -> [ ( Pattern, Value ) ] -> Value
evalC _ _ [ ]				= nonExhaustive
evalC env val ( ( pat, body ) : rest )	= maybe ( evalC env val rest )
	( flip eval body . flip setVars env ) $ match val pat

--------------------------------------------------------------------------------

noVar :: String -> Value
noVar var = Error $ "Not in scope: `" ++ var ++ "'"

notFunction :: Value -> Value
notFunction nf = Error $ "Not Function: " ++ show nf

nonExhaustive :: Value
nonExhaustive = Error "Non-exhaustive patterns in case"
