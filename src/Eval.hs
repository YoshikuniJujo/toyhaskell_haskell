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
eval env ( Letin pats body )	= eval ( setPatsToEnv pats env ) body
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
	( "+", mkBinIntFunction (+) ),
	( "-", mkBinIntFunction (-) ),
	( "*", mkBinIntFunction (*) ),
	( "^", mkBinIntFunction (^) ),
	( "==", mkIntCompFunction (==) ),
	( ":", makeListFun ),
	( ">>", concatMonadFun ),
	( "return", returnFun ),
	( "putChar", Function putCharFun )
 ] emptyEnv

mkBinIntFunction :: ( Integer -> Integer -> Integer ) -> Value
mkBinIntFunction op = Function fun
	where
	fun ( Integer x ) = let
		funN ( Integer y )	= Integer $ x `op` y
		funN ni			= notMatchTypeError "Integer" ni in
		Function funN
	fun ni = notMatchTypeError "Integer" ni

mkIntCompFunction :: ( Integer -> Integer -> Bool ) -> Value
mkIntCompFunction p = Function fun
	where
	fun ( Integer x ) = let
		funN ( Integer y )	= if x `p` y
			then Complex "True" [ ]
			else Complex "False" [ ]
		funN ni			= notMatchTypeError "Integer" ni in
		Function funN
	fun ni = notMatchTypeError "Integer" ni

makeListFun :: Value
makeListFun = Function fun
	where fun v = let funV lst = Complex ":" [ v, lst ] in Function funV

concatMonadFun :: Value
concatMonadFun = Function fun
	where
	fun ( IOAction act1 ) = let
		funA ( IOAction act2 ) = IOAction $ act1 >> act2
		funA v = notMatchTypeError "IO" v in
		Function funA
	fun v = notMatchTypeError "IO" v

returnFun :: Value
returnFun = Function $ IOAction . return

putCharFun :: Value -> Value
putCharFun ( Char c ) = IOAction $ putChar c >> return Nil
putCharFun v = Error $ "putChar :: String -> IO (): " ++ show v

notMatchTypeError :: String -> Value -> Value
notMatchTypeError typ val = Error $
	"Couldn't match expected type `" ++ typ ++ "': " ++ show val
