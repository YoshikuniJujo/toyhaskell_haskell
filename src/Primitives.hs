module Primitives (
	initEnv,
	Table,
	getOpTable
) where

import Types ( Value(..), Env, setsToEnv, emptyEnv, Table, Token( .. ) )

import Text.ParserCombinators.Parsec.Pos
import Text.ParserCombinators.Parsec.Expr
import Data.Char

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

getOpTable :: String -> Table
getOpTable opLst =
	map ( uncurry3 mkAssoc . readOpTable ) $
		concatMap prepOpTable $ lines opLst

prepOpTable :: String -> [ String ]
prepOpTable  str = map ( \op -> fix ++ " " ++ power ++ " " ++ op ) ops
	where
	fix : power : ops = sep str
	sep "" = [ ]
	sep [ x ] = [ [ x ] ]
	sep ( ',' : cs ) = "" : sep ( dropWhile isSpace cs )
	sep ( c : cs )
		| isSpace c	= "" : sep ( dropWhile ( `elem` " ,\t" ) cs )
		| otherwise	= ( c : r ) : rs
		where
		r : rs = sep cs

readOpTable :: String -> ( String, Int, Assoc )
readOpTable str = ( op, read power, assoc )
	where
	[ fix, power, op_ ] = words str
	assoc = case fix of
		"infix"		-> AssocNone
		"infixr"	-> AssocRight
		"infixl"	-> AssocLeft
		_		-> error "bad"
	op = case op_ of
		'`' : o	-> init o
		_	-> op_

uncurry3 :: ( a -> b -> c -> d ) -> ( a, b, c ) -> d
uncurry3 f ( x, y, z ) = f x y z

mkAssoc :: String -> Int -> Assoc ->
	( ( Token, SourcePos ), Value -> Value -> Value, Int, Assoc )
mkAssoc op power assoc =
	( ( Operator op, initialPos "" ), Apply . Apply ( Identifier op ),
		power, assoc )
