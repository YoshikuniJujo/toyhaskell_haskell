module Eval (
	eval,
	initEnv
) where

import Value ( Value( .. ), Pattern( .. ), match,
	Env, emptyEnv, addEnvs, setPatToEnv, setPatsToEnv, setsToEnv, getFromEnv )
import Data.Maybe ( fromMaybe )

--------------------------------------------------------------------------------

eval :: Env -> Value -> Value
eval env ( Identifier i )	=
	eval env $ fromMaybe ( noVarError i ) $ getFromEnv i env
eval env ( Apply f a )		= case eval env f of
	Function fun			-> fun $ eval env a
	Lambda lenv [ pat ] body	->
		eval ( setPatToEnv pat ( eval env a ) lenv `addEnvs` env ) body
	Lambda lenv ( pat : pats ) body	->
		Lambda ( setPatToEnv pat ( eval env a ) lenv ) pats body
	e@( Error _ )			-> e
	_				-> notFunctionError f
eval env ( Letin pvs body )	= eval ( setPatsToEnv pvs env ) body
eval env ( If test thn els )	= case eval env test of
	Complex "True"	[ ]	-> eval env thn
	Complex "False" [ ]	-> eval env els
	nb			-> notBoolError nb
eval env ( Case val bodys )	= patMatch env ( eval env val ) bodys
eval _ v			= v

patMatch :: Env -> Value -> [ ( Pattern, Value ) ] -> Value
patMatch _ _ [ ]				= nonExhaustiveError
patMatch env val ( ( pat, body ) : rest )	=
	maybe ( patMatch env val rest )
		( flip eval body . flip setsToEnv env ) $ match val pat

noVarError :: String -> Value
noVarError var = Error $ "Not in scope: `" ++ var ++ "'"

notFunctionError :: Value -> Value
notFunctionError nf = Error $ "Not Function: " ++ show nf

notBoolError :: Value -> Value
notBoolError nb = Error $ "Not Bool: " ++ show nb

nonExhaustiveError :: Value
nonExhaustiveError = Error "Non-exhaustive patterns in case"

--------------------------------------------------------------------------------

initEnv :: Env
initEnv = setsToEnv [
	( "+",		Function $ mkBinIntFunction (+) ),
	( "-",		Function $ mkBinIntFunction (-) ),
	( "*",		Function $ mkBinIntFunction (*) ),
	( "div",	Function $ mkBinIntFunction div ),
	( "^",		Function $ mkBinIntFunction (^) ),
	( "==",		Function $ mkIntCompFunction (==) ),
	( ":",		Function makeListFun ),
	( ">>",		Function concatMonadFun ),
	( "return",	Function $ IOAction . return ),
	( "putChar", 	Function putCharFun )
 ] emptyEnv

mkIntIntFunction :: ( Integer -> Integer ) -> Value -> Value
mkIntIntFunction fun ( Integer x )	= Integer $ fun x
mkIntIntFunction _ ni			= notMatchTypeError "Integer" ni

mkBinIntFunction :: ( Integer -> Integer -> Integer ) -> Value -> Value
mkBinIntFunction op ( Integer x )	= Function $ mkIntIntFunction $ op x
mkBinIntFunction _ ni			= notMatchTypeError "Integer" ni

mkIntBoolFunction :: ( Integer -> Bool ) -> Value -> Value
mkIntBoolFunction p ( Integer x )	= if p x
	then Complex "True" [ ]
	else Complex "False" [ ]
mkIntBoolFunction _ ni			= notMatchTypeError "Integer" ni

mkIntCompFunction :: ( Integer -> Integer -> Bool ) -> Value -> Value
mkIntCompFunction p ( Integer x )	= Function $ mkIntBoolFunction $ p x
mkIntCompFunction _ ni			= notMatchTypeError "Integer" ni

makeListFun :: Value -> Value
makeListFun v = Function pushV
	where
	pushV lst	= Complex ":" [ v, lst ]

concatMonadFun :: Value -> Value
concatMonadFun ( IOAction act1 ) = Function fun
	where
	fun ( IOAction act2 )	= IOAction $ act1 >> act2
	fun v			= notMatchTypeError "IO" v
concatMonadFun v		= notMatchTypeError "IO" v

putCharFun :: Value -> Value
putCharFun ( Char c )	= IOAction $ putChar c >> return Nil
putCharFun v		= Error $ "putChar :: String -> IO (): " ++ show v

notMatchTypeError :: String -> Value -> Value
notMatchTypeError typ val = Error $
	"Couldn't match expected type `" ++ typ ++ "': " ++ show val
