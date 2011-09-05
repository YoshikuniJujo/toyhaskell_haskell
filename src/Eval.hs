module Eval ( toyEval ) where

import Value ( Value(
	Identifier, Complex, Apply, Function, Lambda, Closure, Case, Letin, Let,
	Module,	Error ),
	Pattern, match, Env, setVars, setPat, setPats, getVal, getPatVars )
import Data.Maybe ( fromMaybe )

--------------------------------------------------------------------------------

toyEval :: Env -> Value -> Value
toyEval env val = case noVars env val of
	[ ]	-> eval env val
	vars	-> Error $ unlines $ map errMsg vars
	where errMsg var = "\tNot in scope: `" ++ var ++ "'"

getVars :: Pattern -> [ String ]
getVars = getPatVars

getV :: Env -> String -> Maybe Value
getV env var = getVal ( eval env ) var env

noVars :: Env -> Value -> [ String ]
noVars env ( Identifier i _ )	= maybe [ i ] ( noVars env ) $ env `getV` i
noVars env ( Apply f a )	= noVars env f ++ noVars env a
noVars env ( Lambda ps expr )	= ps `filterVars` noVars env expr
noVars env ( Case key sels )	= noVars env key ++ noVarsC env `concatMap` sels
noVars env ( Letin defs expr )	= map fst defs `filterVars`
	( noVars env expr ++ ( noVars env . snd ) `concatMap` defs )
noVars env ( Let defs )		= map fst defs `filterVars`
	( ( noVars env . snd ) `concatMap` defs )
noVars env ( Module defs )	= noVars env ( Let defs )
noVars _ _			= [ ]

filterVars :: [ Pattern ] -> [ String ] -> [ String ]
filterVars pats = filter ( `notElem` getVars `concatMap` pats )

noVarsC :: Env -> ( Pattern, Value ) -> [ String ]
noVarsC env ( test, expr ) = ( `notElem` getVars test ) `filter` noVars env expr

eval :: Env -> Value -> Value
eval env ( Identifier i _ )	= eval env $ fromMaybe ( noVar i ) $ getV env i
eval env ( Complex con mems )	= Complex con $ eval env `map` mems
eval env ( Apply f a )		= case eval env f of
	Function fun		-> fun $ eval env a
	Closure ce [ p ] b	-> eval ( setPat p ( eval env a ) ce ) b
	Closure ce ( p : ps ) b	-> Closure ( setPat p ( eval env a ) ce ) ps b
	e@( Error _ )		-> e
	_			-> notFunction f
eval env ( Lambda ps expr )	= Closure env ps expr
eval env ( Case key sels )	= evalC env ( eval env key ) sels
eval env ( Letin defs body )	= eval ( setPats defs env ) body
eval _ v			= v

evalC :: Env -> Value -> [ ( Pattern, Value ) ] -> Value
evalC _ _ [ ]				= nonExhaustive
evalC env key ( ( test, expr ) : sels )	= maybe ( evalC env key sels )
	( flip eval expr . flip setVars env ) $ match key test

--------------------------------------------------------------------------------

noVar :: String -> Value
noVar var = Error $ "Not in scope: `" ++ var ++ "'"

notFunction :: Value -> Value
notFunction nf = Error $ "Not Function: " ++ show nf

nonExhaustive :: Value
nonExhaustive = Error "Non-exhaustive patterns in case"
