module Value (
	Env,
	emptyEnv,
	setToEnv,
	setPatToEnv,
	addEnvs,
	getFromEnv,
	Value( .. ),
	Pattern( .. ),
	showValue,
	isInteger
) where

import Data.Maybe
import Control.Monad

data Env = Env [ ( [ String ], Pattern, Value ) ]
emptyEnv = Env [ ]

setToEnv :: String -> Value -> Env -> Env
setToEnv var val ( Env ps ) =
	Env $ ( [ var ], PatVar var, val ) : ps

setPatToEnv :: Pattern -> Value -> Env -> Env
setPatToEnv pat val ( Env env ) = Env $ ( vars, pat, val ) : env
	where vars = getPatVars pat

getPatVars :: Pattern -> [ String ]
getPatVars ( PatConst _ pats ) = concatMap getPatVars pats
getPatVars ( PatVar var ) = [ var ]
getPatVars _ = [ ]

getFromEnv :: String -> Env -> Maybe Value
getFromEnv var ( Env ps ) = do
	( _, pat, val ) <- usePat
	patMatch1 val pat >>= lookup var
	where
	one ( x, _, _ ) = x
	usePat = listToMaybe $ filter ( ( var `elem` ) . one ) ps

patMatch1 :: Value -> Pattern -> Maybe [ ( String, Value ) ]
patMatch1 ( Integer i1 ) ( PatInteger i0 )
	| i1 == i0	= Just [ ]
	| otherwise	= Nothing
patMatch1 val ( PatVar var )	= Just $ [ ( var, val ) ]
patMatch1 ( Complex name1 bodys ) ( PatConst name0 pats )
	| name1 == name0	=
		liftM ( foldr (++) [ ] ) $
			zipWithM patMatch1 bodys pats
	| otherwise		= Nothing
patMatch1 Empty PatEmpty	= Just [ ]
patMatch1 _ _			= Nothing

addEnvs :: Env -> Env -> Env
addEnvs ( Env ps1 ) ( Env ps2 ) = Env $ ps1 ++ ps2

data Pattern =
	PatConst String [ Pattern ] |
	PatVar { patVar :: String } |
	PatInteger Integer |
	PatEmpty
	deriving Eq

data Value =
	Nil |
	Empty |
	Integer Integer |
	Char Char |
	Bool Bool |
	Complex String [ Value ] |
	Identifier String |
	Function ( Value -> Value ) |
	IOAction ( IO Value ) |
	Apply Value Value |
	Lambda Env [ Pattern ] Value |
	Letin [ ( Pattern, Value ) ] Value |
	Let [ ( Pattern, Value ) ] |
	If Value Value Value |
	Case Value [ ( Pattern, Value ) ] |
	Error String

isInteger :: Value -> Bool
isInteger ( Integer _ )	= True
isInteger _		= False

instance Show Value where
	show Nil = "()"
	show Empty = "[]"
	show ( Integer n )	= show n
	show ( Char c )		= show c
	show ( Bool b )		= show b
	show ( Identifier i )	= i
	show ( Function _ )	= "<function>"
	show ( IOAction _ )	= "<IO>"
	show ( Apply f a )	= "(" ++ show f ++ " " ++ show a ++ ")"
	show ( Lambda _ _ _ )	= "<closure>"
	show ( Letin _ _ )	= "<let-in>"
	show ( Let _ )		= "<let>"
	show ( If _ _ _ )	= "<if>"
	show ( Case _ _ )	= "<case>"
	show v@( Complex ":" [ Char _, _ ] )	= "\"" ++ showStr v ++ "\""
	show v@( Complex ":" _ ) = "[" ++ showL v ++ "]"
	show ( Complex n vs )	= "(" ++ n ++ " " ++ unwords ( map show vs ) ++ ")"
	show ( Error msg )	= "Error: " ++ msg

showL :: Value -> String
showL ( Complex ":" [ v, Empty ] )	= show v
showL ( Complex ":" [ v, c ] )	= show v ++ "," ++ showL c

showStr :: Value -> String
showStr Empty = ""
showStr ( Complex ":" [ Char c, s ] ) = c : showStr s

showValue :: Value -> IO ()
showValue ( IOAction act ) = do
	v <- act
	case v of
		Nil	-> return ()
		_	-> print v
showValue v = print v
