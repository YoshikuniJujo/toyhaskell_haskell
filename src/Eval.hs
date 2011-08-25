module Eval (
	eval
) where

import Types ( Value( .. ), Pattern( .. ), Env, match )
import Env
import Data.Maybe ( fromMaybe )

--------------------------------------------------------------------------------

eval :: Env -> Value -> Value
eval env ( Identifier i )	=
	eval env $ fromMaybe ( noVarError i ) $ getFromEnv ( eval env ) i env
eval env ( Apply f a )		= case eval env f of
	Function fun			-> fun $ eval env a
	Lambda lenv [ pat ] body	->
		eval ( setPatToEnv pat ( eval env a ) lenv `addEnvs` env ) body
	Lambda lenv ( pat : pats ) body	->
		Lambda ( setPatToEnv pat ( eval env a ) lenv ) pats body
	e@( Error _ )			-> e
	_				-> notFunctionError f
eval env ( Letin pvs body )	= eval ( setPatsToEnv pvs env ) body
eval env ( Case val bodys )	= patMatch env ( eval env val ) bodys
eval _ v			= v

patMatch :: Env -> Value -> [ ( Pattern, Value ) ] -> Value
patMatch _ _ [ ]				= nonExhaustiveError
patMatch env val ( ( pat, body ) : rest )	=
	maybe ( patMatch env val rest )
		( flip eval body . flip setsToEnv env ) $ match val pat

--------------------------------------------------------------------------------

noVarError :: String -> Value
noVarError var = Error $ "Not in scope: `" ++ var ++ "'"

notFunctionError :: Value -> Value
notFunctionError nf = Error $ "Not Function: " ++ show nf

nonExhaustiveError :: Value
nonExhaustiveError = Error "Non-exhaustive patterns in case"
