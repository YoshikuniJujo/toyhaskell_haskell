module Eval (
	toyEval
) where

import Value ( Value( .. ), Pattern( .. ), match,
	Env, setVars, setPat, setPats, getVal, getPatVars )
import Data.Maybe ( fromMaybe )

--------------------------------------------------------------------------------

toyEval :: Env -> Value -> Value
toyEval env v = case notSetVars env v of
	[ ]	-> eval env v
	vars	-> Error $ unlines $ map errMsg vars
	where errMsg var = "\tNot in scope: `" ++ var ++ "'"

notSetVars :: Env -> Value -> [ String ]
notSetVars env ( Identifier i _ ) = case getVal ( eval env ) i env of
	Just v	-> notSetVars env v
	Nothing	-> [ i ]
notSetVars env ( Apply f a )		= notSetVars env f ++ notSetVars env a
notSetVars env ( Lambda pats body )	=
	filter ( `notElem` concatMap getPatVars pats ) $ notSetVars env body
notSetVars env ( Case val bodys )	=
	notSetVars env val ++ concatMap ( \( p, v ) ->
		filter ( `notElem` getPatVars p ) $ notSetVars env v ) bodys
notSetVars env ( Letin pvs body )	=
	filter ( `notElem` concatMap ( getPatVars . fst ) pvs ) $
		notSetVars env body
notSetVars env ( Let pvs )		=
	filter ( `notElem` concatMap ( getPatVars . fst ) pvs ) $
		concatMap ( notSetVars env . snd ) pvs
notSetVars _ _			= [ ]

eval :: Env -> Value -> Value
eval env ( Identifier i _ )	=
	eval env $ fromMaybe ( noVarError i ) $ getVal ( eval env ) i env
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
eval env ( Letin pvs body )	= eval ( setPats pvs env ) body
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
