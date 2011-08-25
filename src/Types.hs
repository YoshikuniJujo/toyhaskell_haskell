module Types (
	Token( .. ),
	Pattern( .. ),
	Value( .. ),
	OpTable,

	showValue,
	match,

	Env,
	emptyEnv,
	addEnvs,
	setVars,
	setPat,
	setPats,
	getVal
) where

import Env (
	emptyEnv, addEnvs, setsToEnv, setPatToEnv, setPatsToEnv, getFromEnv )
import qualified Env as E ( Env )
import Text.ParserCombinators.Parsec.Expr ( Assoc )
import Control.Monad ( liftM, zipWithM )

data Token =
	Special Char |
	Variable String | TokConst String | Operator String |
	Backslash | Reserved String | ReservedOp String |
	TokInteger Integer | TokChar Char | TokString String | NewLine
	deriving ( Show, Eq )

data Pattern =
	PatConst String [ Pattern ]	|
	PatVar { patVar :: String }	|
	PatUScore			|
	PatInteger Integer		|
	PatEmpty
	deriving ( Eq, Show )

data Value =
	Nil |
	Empty |
	Integer Integer |
	Char Char |
	Complex String [ Value ] |
	Identifier String |
	Function ( Value -> Value ) |
	IOAction ( IO Value ) |
	Apply Value Value |
	Lambda Env [ Pattern ] Value |
	Letin [ ( Pattern, Value ) ] Value |
	Let [ ( Pattern, Value ) ] |
	Case Value [ ( Pattern, Value ) ] |
	Error String

instance Show Value where
	show Nil		= "()"
	show Empty		= "[]"
	show ( Integer n )	= show n
	show ( Char c )		= show c
	show ( Identifier i )	= i
	show ( Function _ )	= "<function>"
	show ( IOAction _ )	= "<IO>"
	show ( Apply f a )	= "(" ++ show f ++ " " ++ show a ++ ")"
	show ( Lambda _ _ _ )	= "<closure>"
	show ( Letin a b )	= "let " ++ show a ++ " in " ++ show b
	show ( Let _ )		= "<let>"
	show ( Case _ _ )	= "<case>"
	show v@( Complex ":" [ Char _, _ ] )	= "\"" ++ showStr v ++ "\""
	show v@( Complex ":" _ )		= "[" ++ showL v ++ "]"
	show ( Complex n [ ] )	= n
	show ( Complex n vs )	= "(" ++ n ++ " " ++ unwords ( map show vs ) ++ ")"
	show ( Error msg )	= "Error: " ++ msg

showL :: Value -> String
showL ( Complex ":" [ v, Empty ] )	= show v
showL ( Complex ":" [ v, c ] )	= show v ++ "," ++ showL c
showL _ = "Error: bad List"

showStr :: Value -> String
showStr Empty = ""
showStr ( Complex ":" [ Char '\\', s ] ) = '\\' : '\\' : showStr s
showStr ( Complex ":" [ Char '\n', s ] ) = '\\' : 'n' : showStr s
showStr ( Complex ":" [ Char c, s ] ) = c : showStr s
showStr _ = "Error: bad String"

showValue :: Value -> IO ()
showValue ( IOAction act ) = do
	v <- act
	case v of
		Nil	-> return ()
		_	-> print v
showValue v = print v

type Env = E.Env Pattern Value

type OpTable = [ ( String, Int, Assoc ) ]

match :: Value -> Pattern -> Maybe [ ( String, Value ) ]
match ( Integer i1 ) ( PatInteger i0 )
	| i1 == i0	= Just [ ]
	| otherwise	= Nothing
match val ( PatVar var )	= Just [ ( var, val ) ]
match _ PatUScore		= Just [ ]
match ( Complex name1 bodys ) ( PatConst name0 pats )
	| name1 == name0	=
		liftM concat $ zipWithM match bodys pats
	| otherwise		= Nothing
match Empty PatEmpty	= Just [ ]
match _ _			= Nothing

getPatVars :: Pattern -> [ String ]
getPatVars ( PatConst _ pats ) = concatMap getPatVars pats
getPatVars ( PatVar var ) = [ var ]
getPatVars _ = [ ]

setPat :: Pattern -> Value -> Env -> Env
setPat = setPatToEnv getPatVars

setPats :: [ ( Pattern, Value ) ] -> Env -> Env
setPats = setPatsToEnv getPatVars

setVars :: [ ( String, Value ) ] -> Env -> Env
setVars = setsToEnv PatVar

getVal :: ( Value -> Value ) -> String -> Env -> Maybe Value
getVal = getFromEnv match
