{-# Language PackageImports #-}

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
	getVal,

	ParserMonad,
	makeParserInput,
	getSource,
	getSrc,
	putSrc,
	updatePos,
	getCols,
	pushBuf,
	popBuf,
	peekIndents,
	pushIndents,
	popIndents
) where

import Env (
	emptyEnv, addEnvs, setsToEnv, setPatToEnv, setPatsToEnv, getFromEnv )
import qualified Env as E ( Env )
import Text.ParserCombinators.Parsec.Expr ( Assoc )
import Control.Monad ( liftM, zipWithM )
import "monads-tf" Control.Monad.State ( put, get, State )

type ParserMonad = State ( Int, [ Int ], ( Int, Int ), String, [ ( Token, Int ) ] )

getSource :: ( Int, [ Int ], ( Int, Int ), String ) -> String
getSource ( _, _, _, src ) = src

data Token =
	Special Char		|
	ReservedId String	|
	ReservedOp String	|
	Varid String		|
	Conid String		|
	VarSym String		|
	TokInteger Integer	|
	TokChar Char		|
	TokString String	|
	NewLine			|
	TokenEOF		|
	AddBrace Int		|
	Indent Int
	deriving ( Show, Eq )

data Pattern =
	PatConst String [ Pattern ]	|
	PatVar String			|
	PatUScore			|
	PatInteger Integer		|
	PatEmpty
	deriving ( Eq, Show )

data Value =
	Nil					|
	Empty					|
	Identifier String			|
	Integer Integer				|
	Char Char				|
	Complex String [ Value ]		|
	Apply Value Value			|
	Function ( Value -> Value )		|
	IOAction ( IO Value )			|
	Lambda Env [ Pattern ] Value		|
	Case Value [ ( Pattern, Value ) ]	|
	Letin [ ( Pattern, Value ) ] Value	|
	Let [ ( Pattern, Value ) ]		|
	Error String

instance Show Value where
	show Nil		= "()"
	show Empty		= "[]"
	show ( Identifier i )	= i
	show ( Integer n )	= show n
	show ( Char c )		= show c
	show v@( Complex ":" [ Char _, _ ] )
				= "\"" ++ showStr v ++ "\""
	show v@( Complex ":" _ )= "[" ++ showL v ++ "]"
	show ( Complex n [ ] )	= n
	show ( Complex n vs )	= "(" ++ n ++ " " ++ unwords ( map show vs ) ++
					")"
	show ( Apply f a )	= "(" ++ show f ++ " " ++ show a ++ ")"
	show ( Function _ )	= "<function>"
	show ( IOAction _ )	= "<IO>"
	show ( Lambda _ _ _ )	= "<closure>"
	show ( Case _ _ )	= "<case>"
	show ( Letin a b )	= "let " ++ show a ++ " in " ++ show b
	show ( Let a )		= "let " ++ show a
	show ( Error msg )	= "Error: " ++ msg

showL :: Value -> String
showL ( Complex ":" [ v, Empty ] )	= show v
showL ( Complex ":" [ v, c ] )		= show v ++ "," ++ showL c
showL _					= "Error: bad List"

showStr :: Value -> String
showStr Empty					= ""
showStr ( Complex ":" [ Char '\\', s ] )	= '\\' : '\\' : showStr s
showStr ( Complex ":" [ Char '\n', s ] )	= '\\' : 'n' : showStr s
showStr ( Complex ":" [ Char c, s ] )		= c : showStr s
showStr _					= "Error: bad String"

showValue :: Value -> IO ()
showValue ( IOAction act )	= do
	v <- act
	case v of
		Nil	-> return ()
		_	-> print v
showValue v			= print v

type OpTable = [ ( String, Int, Assoc ) ]

--------------------------------------------------------------------------------

type Env = E.Env Pattern Value

match :: Value -> Pattern -> Maybe [ ( String, Value ) ]
match val ( PatVar var )	= Just [ ( var, val ) ]
match _ PatUScore		= Just [ ]
match ( Integer i1 ) ( PatInteger i0 )
	| i1 == i0	= Just [ ]
	| otherwise	= Nothing
match ( Complex name1 vals ) ( PatConst name0 pats )
	| name1 == name0	= liftM concat $ zipWithM match vals pats
	| otherwise		= Nothing
match Empty PatEmpty		= Just [ ]
match _ _			= Nothing

getPatVars :: Pattern -> [ String ]
getPatVars ( PatConst _ pats )	= concatMap getPatVars pats
getPatVars ( PatVar var )	= [ var ]
getPatVars _			= [ ]

setPat :: Pattern -> Value -> Env -> Env
setPat = setPatToEnv getPatVars

setPats :: [ ( Pattern, Value ) ] -> Env -> Env
setPats = setPatsToEnv getPatVars

setVars :: [ ( String, Value ) ] -> Env -> Env
setVars = setsToEnv PatVar

getVal :: ( Value -> Value ) -> String -> Env -> Maybe Value
getVal = getFromEnv match

getSrc :: ParserMonad String
getSrc = do
	( _, _, _, src, _ ) <- get
	return src

putSrc :: String -> ParserMonad ()
putSrc src = do
	( idnt1, idnta, pos, _, buf ) <- get
	put ( idnt1, idnta, pos, src, buf )

updatePos :: String -> ParserMonad ()
updatePos str = do
	( idnt1, idnta, ( lns, cols ), src, buf ) <- get
	let ( nlns, ncols ) = up lns cols str
	put ( idnt1, idnta, ( nlns, ncols ), src, buf )
	where
	up l c ""		= ( l, c )
	up l _ ( '\n' : cs )	= up ( l + 1 ) 1 cs
	up l c ( '\t' : cs )	= up l ( 8 * ( c `div` 8 + 1 ) + 1 ) cs
	up l c ( _ : cs )	= up l ( c + 1 ) cs

getCols :: ParserMonad Int
getCols = do
	( _, _, ( _, cols ), _, _ ) <- get
	return cols

pushBuf :: ( Token, Int ) -> ParserMonad ()
pushBuf t = do
	( idnt1, idnta, pos, src, buf ) <- get
	put ( idnt1, idnta, pos, src, t : buf )

popBuf :: ParserMonad ( Maybe ( Token, Int ) )
popBuf = do
	( idnt1, idnta, pos, src, buf@( ~( t : ts ) ) ) <- get
	if null buf then return Nothing else do
		put ( idnt1, idnta, pos, src, ts )
		return $ Just t

peekIndents :: ParserMonad ( Maybe Int )
peekIndents = do
	ma <- getIndents
	case ma of
		m : _	-> return $ Just m
		_	-> return Nothing

pushIndents :: Int -> ParserMonad ()
pushIndents m = do
	ma <- getIndents
	putIndents $ m : ma

popIndents :: ParserMonad ( Maybe Int )
popIndents = do
	ma <- getIndents
	case ma of
		m : ms	-> putIndents ms >> return ( Just m )
		_	-> return Nothing

putIndents :: [ Int ] -> ParserMonad ()
putIndents idnta = do
	( idnt1, _, pos, src, buf ) <- get
	put ( idnt1, idnta, pos, src, buf )

getIndents :: ParserMonad [ Int ]
getIndents = do
	( _idnt1, idnta, _pos, _src, _buf ) <- get
	return idnta

makeParserInput src = ( 0, [ ], ( 1, 1 ), src, [ ] )
