module Primitives (
	initEnv
) where

import Types ( Value( .. ), Env, setVars, emptyEnv )

initEnv :: Env
initEnv = setVars [
	( "+",		Function $ mkBinIntFunction (+) ),
	( "-",		Function $ mkBinIntFunction (-) ),
	( "*",		Function $ mkBinIntFunction (*) ),
	( "div",	Function $ mkBinIntFunction div ),
	( "^",		Function $ mkBinIntFunction (^) ),
	( "==",		Function $ mkIntCompFunction (==) ),
	( ":",		Function makeListFun ),
	( ">>",		Function concatMonadFun ),
	( "return",	Function $ IOAction . return ),
	( "putChar",	Function putCharFun ),
	( "add",	Function $ mkBinIntFunction (+) )
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
makeListFun v = let pushV lst = Complex ":" [ v, lst ] in Function pushV

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
