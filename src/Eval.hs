module Eval ( toyEval ) where

import Value ( Value(
	Identifier, Apply, Function, Lambda, Closure, Case, Letin, Let,	Module,
	Error ),
	Pattern, match, Env, setVars, setPat, setPats, getPatVars )
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

getVal :: Env -> String -> Maybe Value
getVal env var = Value.getVal ( eval env ) var env

filterVars :: [ Pattern ] -> [ String ] -> [ String ]
filterVars pats vars = ( `notElem` getVars `concatMap` pats ) `filter` vars

noVars :: Env -> Value -> [ String ]
noVars env ( Identifier i _ )	= maybe [ i ] ( noVars env ) $ getVal env i
noVars env ( Apply f a )	= noVars env f ++ noVars env a
noVars env ( Lambda vs body )	= vs `filterVars` noVars env body
noVars env ( Case v sel )	= noVars env v ++ noVarsCase env `concatMap` sel
noVars env ( Letin defs body )	= map fst defs `filterVars`
	( noVars env body ++ ( noVars env . snd ) `concatMap` defs )
noVars env ( Let defs )		= map fst defs `filterVars`
	( ( noVars env . snd ) `concatMap` defs )
noVars env ( Module defs )	= noVars env ( Let defs )
noVars _ _			= [ ]

noVarsCase :: Env -> ( Pattern, Value ) -> [ String ]
noVarsCase env ( pat, val ) = ( `notElem` getVars pat ) `filter` noVars env val

eval :: Env -> Value -> Value
eval env ( Identifier i _ )	=
	eval env $ fromMaybe ( noVarError i ) $ getVal env i
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
