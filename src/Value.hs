{-# LANGUAGE TupleSections #-}

module Value (
	Value( .. ),
	Pattern( .. ),
	patVars,
	match,

	Env,
	Var( V ),
	initialize,
	setVars,
	setPat,
	setPats,
	getVal,
	getVars,
	mapEnv
) where

import Prelude hiding ( showList )

import Env ( Var( V ), getVars, mapEnv )
import qualified Env ( Env, initialize, setVars, setPat, setPats, getVal )
import Control.Monad ( liftM, zipWithM )

--------------------------------------------------------------------------------

data Value =
	Nil					|
	Empty					|
	Integer Integer				|
	Char Char				|
	Fun ( Value -> Value )			|
	IOAction ( IO Value )			|
	Var String Int				|
	Comp String [ Value ]			|
	App Value Value				|
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
	show ( Var i 0 )	= i
	show ( Var i n )	= i ++ "~" ++ show n
	show ( Integer n )	= show n
	show ( Char c )		= show c
	show v@( Comp ":" [ Char _, _ ] )
				= "\"" ++ showStr v ++ "\""
	show v@( Comp ":" _ )= "[" ++ showList v ++ "]"
	show ( Comp n [ ] )	= n
	show ( Comp n vs )	= "(" ++ n ++ " " ++ unwords ( map show vs ) ++
					")"
	show ( App f a )	= "( " ++ show f ++ " " ++ show a ++ " )"
	show ( Fun _ )		= "<function>"
	show ( IOAction _ )	= "<IO>"
	show ( Lambda vs body )	= showLambda vs body
	show ( Closure _ _ _ )	= "<closure>"
	show ( Case v ps )	= showCase v ps
	show ( Letin a b )	= "let " ++ showPair a ++ " in " ++ show b
	show ( Module m )	= "module " ++
		unwords ( map ( \( p, v ) -> showPattern p ++ " = " ++ show v ++
		"; " ) m )
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
showPattern ( PatVar var 0 )	= var
showPattern ( PatVar var n )	= var ++ "~" ++ show n
showPattern p			= show p

showList :: Value -> String
showList ( Comp ":" [ v, Empty ] )	= show v
showList ( Comp ":" [ v, c ] )		= show v ++ "," ++ showList c
showList _				= "Error: bad List"

showStr :: Value -> String
showStr Empty					= ""
showStr ( Comp ":" [ Char '\\', s ] )	= '\\' : '\\' : showStr s
showStr ( Comp ":" [ Char '\n', s ] )	= '\\' : 'n' : showStr s
showStr ( Comp ":" [ Char c, s ] )		= c : showStr s
showStr _					= "Error: bad String"

data Pattern =
	PatVar String Int		|
	PatCon String [ Pattern ]	|
	PatInteger Integer		|
	PatUScore			|
	PatEmpty
	deriving ( Eq, Show )

match :: Value -> Pattern -> Maybe [ ( Var, Value ) ]
match val ( PatVar var n )	= Just [ ( V var n, val ) ]
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
patVars = map ( \( V x _ ) -> x ) . patToVars

patToVars :: Pattern -> [ Var ]
patToVars ( PatCon _ pats )	= patToVars `concatMap` pats
patToVars ( PatVar var n )	= [ V var n ]
patToVars _			= [ ]

--------------------------------------------------------------------------------

type Env = Env.Env Pattern Value

initialize :: [ ( String, Value ) ] -> Env
initialize = Env.initialize ( flip PatVar 0 )

setVars :: Env -> [ ( Var, Value ) ] -> Env
setVars = Env.setVars ( \( V x n ) -> PatVar x n )

setPat :: Env -> Pattern -> Value -> Env
setPat = Env.setPat patToVars

setPats :: Env -> [ ( Pattern, Value ) ] -> Env
setPats = Env.setPats patToVars

getVal :: ( Value -> Value ) -> Env -> Var -> Maybe Value
getVal = Env.getVal match
