module Eval (
	eval
) where

import Value ( Value( .. ), Pattern( .. ), match,
	Env, setVars, setPat, setPats, getVal, addEnvs )
import Data.Maybe ( fromMaybe )

--------------------------------------------------------------------------------

eval :: Env -> Value -> Value
eval env ( Identifier i )	=
	eval env $ fromMaybe ( noVarError i ) $ getVal ( eval env ) i env
eval env ( Lambda _ pats body )	= Closure env pats body
eval env ( Apply f a )		= case eval env f of
	Function fun			-> fun $ eval env a
	Closure lenv [ pat ] body	->
		eval ( setPat pat ( eval env a ) lenv ) body
	Closure lenv ( pat : pats ) body	->
		Closure ( setPat pat ( eval env a ) lenv ) pats body
	e@( Error _ )			-> e
	_				-> notFunctionError f
eval env ( Letin pvs body )	= eval ( setPats pvs env ) body
eval env ( Case val bodys )	= patMatch env ( eval env val ) bodys
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
