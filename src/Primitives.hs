module Primitives ( primitives ) where

import Value (
	Value( Nil, Integer, Char, Comp, Fun, IOAction, Error ),
	Env, initialize )

primitives :: Env
primitives = initialize [
	( "+",		Fun $ mkBinIntFunction (+) ),
	( "-",		Fun $ mkBinIntFunction (-) ),
	( "*",		Fun $ mkBinIntFunction (*) ),
	( "div",	Fun $ mkBinIntFunction div ),
	( "^",		Fun $ mkBinIntFunction (^) ),
	( "==",		Fun $ mkIntCompFunction (==) ),
	( ":",		Fun makeList ),
	( ">>",		Fun concatMonad ),
	( "return",	Fun $ IOAction . return ),
	( "putChar",	Fun putCharFun )
 ]

mkIntIntFunction :: ( Integer -> Integer ) -> Value -> Value
mkIntIntFunction fun ( Integer x )	= Integer $ fun x
mkIntIntFunction _ ni			= notMatchTypeError "Integer" ni

mkBinIntFunction :: ( Integer -> Integer -> Integer ) -> Value -> Value
mkBinIntFunction op ( Integer x )	= Fun $ mkIntIntFunction $ op x
mkBinIntFunction _ ni			= notMatchTypeError "Integer" ni

mkIntBoolFunction :: ( Integer -> Bool ) -> Value -> Value
mkIntBoolFunction p ( Integer x )	= if p x	then Comp "True" [ ]
							else Comp "False" [ ]
mkIntBoolFunction _ ni			= notMatchTypeError "Integer" ni

mkIntCompFunction :: ( Integer -> Integer -> Bool ) -> Value -> Value
mkIntCompFunction p ( Integer x )	= Fun $ mkIntBoolFunction $ p x
mkIntCompFunction _ ni			= notMatchTypeError "Integer" ni

makeList :: Value -> Value
makeList h = Fun $ \t -> Comp ":" [ h, t ]

concatMonad :: Value -> Value
concatMonad ( IOAction act1 ) = Fun fun
	where
	fun ( IOAction act2 )	= IOAction $ act1 >> act2
	fun v			= notMatchTypeError "IO" v
concatMonad v			= notMatchTypeError "IO" v

putCharFun :: Value -> Value
putCharFun ( Char c )	= IOAction $ putChar c >> return Nil
putCharFun v		= Error $ "putChar :: String -> IO (): " ++ show v

notMatchTypeError :: String -> Value -> Value
notMatchTypeError typ val = Error $
	"Couldn't match expected type `" ++ typ ++ "': " ++ show val
