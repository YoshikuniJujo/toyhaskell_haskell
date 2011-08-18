module Eval (
	eval,
	initEnv
) where

import Value ( Value( .. ), Pattern( .. ),
	Env, emptyEnv, addEnvs, setPatToEnv, setPatsToEnv, setToEnv, getFromEnv )

import Control.Monad ( liftM, zipWithM )
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
	_				-> notFunctionError f
eval env ( Letin pats body )	= eval ( setPatsToEnv pats env ) body
eval env ( If test thn els )	= case eval env test of
	Complex "True"	[ ]	-> eval env thn
	Complex "False" [ ]	-> eval env els
	_		-> Error $ "Not Bool: " ++ show test
eval env ( Case val bodys )	= patMatch env ( eval env val ) bodys
eval _ v			= v

noVarError :: String -> Value
noVarError var = Error $ "Not in scope: `" ++ var ++ "'"

notFunctionError :: Value -> Value
notFunctionError nf = Error $ "Not Function " ++ show nf

patMatch :: Env -> Value -> [ ( Pattern, Value ) ] -> Value
patMatch _ _ [ ]		= Error "Non-exhaustive patterns in case"
patMatch env val ( ( pat, body ) : rest ) =
	maybe ( patMatch env val rest )
		( \lenv -> eval ( lenv `addEnvs` env ) body ) $ patMatch1 val pat

patMatch1 :: Value -> Pattern -> Maybe Env
patMatch1 ( Integer i1 ) ( PatInteger i0 )
	| i1 == i0	= Just emptyEnv
	| otherwise	= Nothing
patMatch1 val ( PatVar var )	= Just $ setToEnv var val emptyEnv -- Just [ ( var, val ) ]
patMatch1 ( Complex name1 bodys ) ( PatConst name0 pats )
	| name1 == name0	=
		liftM ( foldr addEnvs emptyEnv ) $
			zipWithM patMatch1 bodys pats
	| otherwise		= Nothing
patMatch1 Empty PatEmpty	= Just emptyEnv -- [ ]
patMatch1 _ _			= Nothing

--------------------------------------------------------------------------------

initEnv :: Env
initEnv = foldr ( uncurry setToEnv ) emptyEnv [
	( "+", mkBinIntFunction (+) ),
	( "-", mkBinIntFunction (-) ),
	( "*", mkBinIntFunction (*) ),
	( "^", mkBinIntFunction (^) ),
	( "putChar", Function putCharFun ),
	( "==", mkIntCompFunction (==) ),
	( ":", makeListFun ),
	( ">>", concatMonadFun ),
	( "return", returnFun )
 ]

mkBinIntFunction :: ( Integer -> Integer -> Integer ) -> Value
mkBinIntFunction op = Function fun
	where
	fun ( Integer x ) = let
		funN vy =
			case vy of
				Integer y -> Integer $ x `op` y
				_ -> Error "bad type" in
		Function funN
	fun _ = Error "bad type"

mkIntCompFunction :: ( Integer -> Integer -> Bool ) -> Value
mkIntCompFunction p = Function fun
	where
	fun ( Integer x ) = let
		funN vy =
			case vy of
				Integer y -> if x `p` y
					then Complex "True" [ ]
					else Complex "False" [ ]
				_ -> Error "bad type" in
		Function funN
	fun _ = Error "bad type"

putCharFun :: Value -> Value
putCharFun ( Char c ) = IOAction $ putChar c >> return Nil
putCharFun v = Error $ "putChar :: String -> IO (): " ++ show v

makeListFun :: Value
makeListFun = Function fun
	where
	fun v = let funV lst = Complex ":" [ v, lst ] in
		Function funV

concatMonadFun :: Value
concatMonadFun = Function fun
	where
	fun ( IOAction act1 ) =
		let	funA ( IOAction act2 ) = IOAction $ act1 >> act2
			funA v = Error $ "not IOAction: " ++ show v in
			Function funA
	fun v = Error $ "not IOAction: " ++ show v

returnFun :: Value
returnFun = Function fun
	where
	fun v = IOAction $ return v
