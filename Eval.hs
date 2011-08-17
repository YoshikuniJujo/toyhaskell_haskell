{-# Language PackageImports #-}

module Eval (
	eval,
	initEnv
) where

import Value
import "monads-tf" Control.Monad.Error
import "monads-tf" Control.Monad.Error.Class

initEnv :: Env
initEnv = [
	( "+", mkBinIntFunction (+) ),
	( "-", mkBinIntFunction (-) ),
	( "putStrLn", Function putStrLnFun )
 ]

data MyError = MyError String

instance Show MyError where
	show ( MyError errMsg ) = "Error: " ++ errMsg

instance Error MyError where
	noMsg = MyError "error occur"
	strMsg = MyError

eval :: Env -> Value -> Either MyError Value
eval env ( Identifier i ) =
	maybe ( Left $ MyError $ "Not in scope: `" ++ i ++ "'" ) Right $
	lookup i env
eval env ( Apply f a ) = do
	mfun <- eval env f
	case mfun of
		Function fun			-> do
			arg <- eval env a
			return $ fun arg
		Lambda lenv [ var ] body	-> do
			arg <- eval env a
			let nenv = ( var, arg ) : lenv ++ env
			eval nenv body
		Lambda lenv ( var : vars ) body	-> do
			arg <- eval env a
			let nlenv = ( var, arg ) : lenv
			return $ Lambda nlenv vars body
		_				->
			throwError $ MyError $ "Not Function: " ++ show f
		
eval env v = Right v

mkBinIntFunction :: ( Integer -> Integer -> Integer ) -> Value
mkBinIntFunction op = Function fun
	where
	fun ( Integer x ) = let
		funN ( Integer y ) = Integer $ x `op` y in
		Function funN

putStrLnFun :: Value -> Value
putStrLnFun ( String str ) = IOAction $ putStrLn str >> return Nil
