module Eval ( toyEval ) where

import Value ( Value(
	Identifier, Apply, Function, Lambda, Closure, Case, Letin, Let,	Module,
	Error ),
	Pattern, match, Env, setVars, setPat, setPats, {- getVal, -} getPatVars )
import qualified Value ( getVal )
import Data.Maybe ( fromMaybe )

--------------------------------------------------------------------------------

toyEval :: Env -> Value -> Value
toyEval env v = case noVars env v of
	[ ]	-> eval env v
	vars	-> Error $ unlines $ map errMsg vars
	where errMsg var = "\tNot in scope: `" ++ var ++ "'"

getVars :: Pattern -> [ String ]
getVars = getPatVars

getVal :: String -> Env -> Maybe Value
getVal var env = Value.getVal ( eval env ) var env

notElemPats :: String -> [ Pattern ] -> Bool
notElemPats var pats = var `notElem` getVars `concatMap` pats

filterVarsPats :: [ Pattern ] -> [ String ] -> [ String ]
filterVarsPats pats vars = ( `notElemPats` pats ) `filter` vars

noVars :: Env -> Value -> [ String ]
noVars env ( Identifier i _ )	= maybe [ i ] ( noVars env ) $ getVal i env
noVars env ( Apply f a )	= noVars env f ++ noVars env a
noVars env ( Lambda vs body )	= vs `filterVarsPats` noVars env body
noVars env ( Case v s )		= noVars env v ++ noVarsCase env `concatMap` s
noVars env ( Letin defs body )	= map fst defs `filterVarsPats`
	( noVars env body ++ ( noVars env . snd ) `concatMap` defs )
noVars env ( Let defs )		= map fst defs `filterVarsPats`
	( ( noVars env . snd ) `concatMap` defs )
noVars env ( Module defs )	= noVars env ( Let defs )
noVars _ _			= [ ]

noVarsCase :: Env -> ( Pattern, Value ) -> [ String ]
noVarsCase env ( pat, val ) = ( `notElem` getVars pat ) `filter` noVars env val

eval :: Env -> Value -> Value
eval env ( Identifier i _ )	=
	eval env $ fromMaybe ( noVarError i ) $ getVal i env
eval env ( Apply f a )		= case eval env f of
	Function fun				-> fun $ eval env a
	Closure lenv [ pat ] body		->
		eval ( setPat pat ( eval env a ) lenv ) body
	Closure lenv ( pat : pats ) body	->
		Closure ( setPat pat ( eval env a ) lenv ) pats body
	e@( Error _ )			-> e
	_				-> notFunctionError f
eval env ( Lambda pats body )	= Closure env pats body
eval env ( Case val bodys )	= patMatch env ( eval env val ) bodys
eval env ( Letin defs body )	= eval ( setPats defs env ) body
eval _ v			= v

patMatch :: Env -> Value -> [ ( Pattern, Value ) ] -> Value
patMatch _ _ [ ]				= nonExhaustiveError
patMatch env val ( ( pat, body ) : rest )	=
	maybe ( patMatch env val rest )
		( flip eval body . flip setVars env ) $ match val pat

--------------------------------------------------------------------------------

noVarError :: String -> Value
noVarError var = Error $ "Not in scope: `" ++ var ++ "'"

notFunctionError :: Value -> Value
notFunctionError nf = Error $ "Not Function: " ++ show nf

nonExhaustiveError :: Value
nonExhaustiveError = Error "Non-exhaustive patterns in case"
