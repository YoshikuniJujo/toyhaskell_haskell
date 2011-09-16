module Value (
	Pattern( .. ),
	Value( .. ),

	match,

	Env,
	initialize,
	addEnvs,
	setVars,
	setPat,
	setPats,
	getVal,
	getVars,
	patVars,

	mapEnv
) where

import Env ( getVarsEnv, initEnv,
	addEnvs, setsToEnv, setPatToEnv, setPatsToEnv, getFromEnv, mapEnv )
import qualified Env as E ( Env )
import Control.Monad ( liftM, zipWithM )

data Pattern =
	PatVar String Int		|
	PatCon String [ Pattern ]	|
	PatInteger Integer		|
	PatUScore			|
	PatEmpty
	deriving ( Eq, Show )

data Value =
	Nil					|
	Empty					|
	Integer Integer				|
	Char Char				|
	Fun ( Value -> Value )		|
	IOAction ( IO Value )			|
	Var String Int				|
	Comp String [ Value ]		|
	App Value Value			|
	Lambda [ Pattern ] Value		|
	Closure Env [ Pattern ] Value		|
	Case Value [ ( Pattern, Value ) ]	|
	Letin [ ( Pattern, Value ) ] Value	|
	Module [ ( Pattern, Value ) ]		|
	Let [ ( Pattern, Value ) ]		|
	Error String

instance Show Value where
	show Nil		= "()"
	show Empty		= "[]"
	show ( Var i _ )	= i -- ++ show n
	show ( Integer n )	= show n
	show ( Char c )		= show c
	show v@( Comp ":" [ Char _, _ ] )
				= "\"" ++ showStr v ++ "\""
	show v@( Comp ":" _ )= "[" ++ showL v ++ "]"
	show ( Comp n [ ] )	= n
	show ( Comp n vs )	= "(" ++ n ++ " " ++ unwords ( map show vs ) ++
					")"
	show ( App f a )	= "( " ++ show f ++ " " ++ show a ++ " )"
	show ( Fun _ )		= "<function>"
	show ( IOAction _ )	= "<IO>"
	show ( Lambda vs body )	= showLambda vs body -- "<lambda>"
	show ( Closure _ _ _ )	= "<closure>"
	show ( Case v ps )	= showCase v ps
	show ( Letin a b )	= "let " ++ showPair a ++ " in " ++ show b
	show ( Module _ )	= "<module>"
	show ( Let a )		= "let " ++
		unwords ( map ( \( p, v ) -> showPattern p ++ " = " ++ show v ++
		"; " ) a )
	show ( Error msg )	= "Error: " ++ msg

showPair :: [ ( Pattern, Value ) ] -> String
showPair a = unwords ( map ( \( p, v ) -> showPattern p ++ " = " ++ show v ++
	";" ) a )

showLambda :: [ Pattern ] -> Value -> String
showLambda vs body = "( \\" ++ unwords ( map showPattern vs ) ++ " -> " ++
	show body ++ " )"

showCase :: Value -> [ ( Pattern, Value ) ] -> String
showCase v ps = "case " ++ show v ++ " of { " ++
	unwords ( map ( \( p, v' ) -> showPattern p ++ " -> " ++ show v' ++ "; " ) ps )
	++ " }"

showPattern :: Pattern -> String
showPattern ( PatVar var _ )	= var -- ++ show n
showPattern p			= show p

showL :: Value -> String
showL ( Comp ":" [ v, Empty ] )	= show v
showL ( Comp ":" [ v, c ] )		= show v ++ "," ++ showL c
showL _					= "Error: bad List"

showStr :: Value -> String
showStr Empty					= ""
showStr ( Comp ":" [ Char '\\', s ] )	= '\\' : '\\' : showStr s
showStr ( Comp ":" [ Char '\n', s ] )	= '\\' : 'n' : showStr s
showStr ( Comp ":" [ Char c, s ] )		= c : showStr s
showStr _					= "Error: bad String"

--------------------------------------------------------------------------------

type Env = E.Env Pattern Value

match :: Value -> Pattern -> Maybe [ ( String, Value ) ]
match val ( PatVar var _ )	= Just [ ( var, val ) ]
match _ PatUScore		= Just [ ]
match ( Integer i1 ) ( PatInteger i0 )
	| i1 == i0	= Just [ ]
	| otherwise	= Nothing
match ( Comp name1 vals ) ( PatCon name0 pats )
	| name1 == name0	= liftM concat $ zipWithM match vals pats
	| otherwise		= Nothing
match Empty PatEmpty		= Just [ ]
match _ _			= Nothing

patVars :: Pattern -> [ String ]
patVars = getPatVars

getPatVars :: Pattern -> [ String ]
getPatVars ( PatCon _ pats )	= concatMap getPatVars pats
getPatVars ( PatVar var _ )	= [ var ]
getPatVars _			= [ ]

initialize :: [ ( String, Value ) ] -> Env
initialize = initEnv $ flip PatVar 0

setPat :: Pattern -> Value -> Env -> Env
setPat = setPatToEnv getPatVars

setPats :: [ ( Pattern, Value ) ] -> Env -> Env
setPats = setPatsToEnv getPatVars

setVars :: [ ( String, Value ) ] -> Env -> Env
setVars = setsToEnv ( flip PatVar 0 )

getVars :: Env -> [ String ]
getVars = getVarsEnv

getVal :: ( Value -> Value ) -> String -> Env -> Maybe Value
getVal = getFromEnv match
