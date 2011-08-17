{-# Language PackageImports #-}

module Eval (
	eval,
	initEnv
) where

import Value
import "monads-tf" Control.Monad.Error
import "monads-tf" Control.Monad.Error.Class
import Data.Maybe

initEnv :: Env
initEnv = [
	( "+", mkBinIntFunction (+) ),
	( "-", mkBinIntFunction (-) ),
	( "*", mkBinIntFunction (*) ),
	( "putStrLn", Function putStrLnFun ),
	( "==", mkIntCompFunction (==) )
 ]

data MyError = MyError String

instance Show MyError where
	show ( MyError errMsg ) = "Error: " ++ errMsg

instance Error MyError where
	noMsg = MyError "error occur"
	strMsg = MyError

eval :: Env -> Value -> Value
eval env ( Identifier i ) =
	eval env $ fromMaybe ( Error $ "Not in scope: `" ++ i ++ "'" ) $ lookup i env
eval env ( Apply f a ) = let
	mfun = eval env f
	ret = case mfun of
		Function fun			->
			let	arg = eval env a in
				fun arg
		Lambda lenv [ var ] body	->
			let	arg = eval env a
				nenv = ( var, arg ) : lenv ++ env in
				eval nenv body
		Lambda lenv ( var : vars ) body	->
			let	arg = eval env a
				nlenv = ( var, arg ) : lenv in
				Lambda nlenv vars body
		_				->
			Error $ "Not Function: " ++ show f in
	ret
eval env ( Letin ps body ) = let
	nenv = ps ++ env in
	eval nenv body
eval env ( If test thn els ) = case eval env test of
	Bool True	-> eval env thn
	Bool False	-> eval env els
	_		-> Error $ "Not Bool: " ++ show test
		
eval _ v = v

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
				Integer y -> Bool $ x `p` y
				_ -> Error "bad type" in
		Function funN
	fun _ = Error "bad type"

putStrLnFun :: Value -> Value
putStrLnFun ( String str ) = IOAction $ putStrLn str >> return Nil
putStrLnFun v = Error $ "putStrLn :: String -> IO (): " ++ show v
