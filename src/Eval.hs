module Eval (
	eval,
	checkAndEval,
	getNotSetVars
) where

import Value ( Value( .. ), Pattern( .. ), match,
	Env, setVars, setPat, setPats, getVal, getPatVars )
import Data.Maybe ( fromMaybe )

--------------------------------------------------------------------------------

checkAndEval :: Env -> Value -> Value
checkAndEval env v = case getNotSetVars env v of
	[ ]	-> eval env v
	vars	-> Error $ mkErrMsg $ head vars
	where
	mkErrMsg var = "Not in scope: `" ++ var ++ "'"

getNotSetVars :: Env -> Value -> [ String ]
getNotSetVars env ( Identifier i ) = case getVal ( eval env ) i env of
	Just v	-> getNotSetVars env v
	Nothing	-> [ i ]
getNotSetVars env ( Lambda pats body )	=
	filter ( `notElem` concatMap getPatVars pats ) $ getNotSetVars env body
getNotSetVars env ( Apply f a )		= getNotSetVars env f ++ getNotSetVars env a
getNotSetVars env ( Letin pvs body )	=
	filter ( `notElem` concatMap ( getPatVars . fst ) pvs ) $ getNotSetVars env body
getNotSetVars env ( Case val bodys )	=
	getNotSetVars env val ++ concatMap ( \( p, v ) ->
		filter ( `notElem` getPatVars p ) $ getNotSetVars env v ) bodys
getNotSetVars env ( Let pvs )		=
	filter ( `notElem` concatMap ( getPatVars . fst ) pvs ) $
		concatMap ( getNotSetVars env . snd ) pvs
getNotSetVars _ _			= [ ]

eval :: Env -> Value -> Value
eval env ( Identifier i )	=
	eval env $ fromMaybe ( noVarError i ) $ getVal ( eval env ) i env
eval env ( Lambda pats body )	= Closure env pats body
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
