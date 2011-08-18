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

data MyError = MyError String

instance Show MyError where
	show ( MyError errMsg ) = "Error: " ++ errMsg

instance Error MyError where
	noMsg = MyError "error occur"
	strMsg = MyError

eval :: Env -> Value -> Value
eval env ( Identifier i ) =
	eval env $ fromMaybe ( Error $ "Not in scope: `" ++ i ++ "'" ) $
		getFromEnv i env
eval env ( Apply f a ) = let
	mfun = eval env f
	ret = case mfun of
		Function fun			->
			let	arg = eval env a in
				fun arg
		Lambda lenv [ PatVar var ] body	->
			let	arg = eval env a
				nenv = setToEnv var arg $ lenv `addEnvs` env in
				eval nenv body
		Lambda lenv ( PatVar var : vars ) body	->
			let	arg = eval env a
				nlenv = setToEnv var arg lenv in
				Lambda nlenv vars body
		_				->
			Error $ "Not Function: " ++ show f in
	ret
eval env ( Letin pats body ) = let
--	ps = foldr ( uncurry setToEnv ) emptyEnv $ map ( first patVar ) pats
	nenv = foldr ( uncurry setPatToEnv ) env pats in -- ps `addEnvs` env in
	eval nenv body
eval env ( If test thn els ) = case eval env test of
	Bool True	-> eval env thn
	Bool False	-> eval env els
	_		-> Error $ "Not Bool: " ++ show test
eval env ( Case val bodys ) = patMatch env ( eval env val ) bodys
eval _ v = v

patMatch :: Env -> Value -> [ ( Pattern, Value ) ] -> Value
patMatch _ _ [ ]		= Error "Non-exhaustive patterns in case"
patMatch env val ( ( pat, body ) : rest ) =
	maybe ( patMatch env val rest )
		( \lenv -> eval ( lenv `addEnvs` env ) body ) $ patMatch1 val pat

-- patMatchEnv :: [ ( Pattern, Value ) ] -> [ ( [ String ], Maybe Env ) ]
-- patMatchEnv = sequence . map ( uncurry $ flip patMatch1 )

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
