module Primitives (primitives) where

import Value (
	Value(Nil, Integer, Char, Con, Fun, IOAction, Err), Env, initialize)

primitives :: Env
primitives = initialize [
	("+",		Fun $ mkBinIntFun (+)),
	("-",		Fun $ mkBinIntFun (-)),
	("*",		Fun $ mkBinIntFun (*)),
	("div",	Fun $ mkBinIntFun div),
	("^",		Fun $ mkBinIntFun (^)),
	("==",		Fun $ mkCompIntFun (==)),
	(">>",		Fun concatMonad),
	("return",	Fun $ IOAction . return),
	("putChar",	Fun putCharFun)
 ]

mkIntFun :: (Integer -> Integer) -> Value -> Value
mkIntFun f (Integer x)		= Integer $ f x
mkIntFun _ v			= typeError "Integer" v

mkBinIntFun :: (Integer -> Integer -> Integer) -> Value -> Value
mkBinIntFun op (Integer x)	= Fun $ mkIntFun $ op x
mkBinIntFun _ v			= typeError "Integer" v

mkIntBoolFun :: (Integer -> Bool) -> Value -> Value
mkIntBoolFun p (Integer x)
	| p x			= Con "True" []
	| otherwise		= Con "False" []
mkIntBoolFun _ v		= typeError "Integer" v

mkCompIntFun :: (Integer -> Integer -> Bool) -> Value -> Value
mkCompIntFun p (Integer x)	= Fun $ mkIntBoolFun $ p x
mkCompIntFun _ v		= typeError "Integer" v

concatMonad :: Value -> Value
concatMonad (IOAction act1)	= Fun fun
	where
	fun (IOAction act2)	= IOAction $ act1 >> act2
	fun v			= typeError "IO" v
concatMonad v			= typeError "IO" v

putCharFun :: Value -> Value
putCharFun (Char c)		= IOAction $ putChar c >> return Nil
putCharFun v			= typeError "Char" v

typeError :: String -> Value -> Value
typeError t v = Err $ "Couldn't match expected type `" ++ t ++ "': " ++ show v
