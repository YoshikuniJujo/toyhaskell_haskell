module Value (
	Env,
	Value( .. ),
	Pattern( .. ),
	showValue,
	isInteger
) where

type Env = [ ( String, Value ) ]

data Pattern =
	PatConst String [ Pattern ] |
	PatVar { patVar :: String } |
	PatInteger Integer
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
	show v@( Complex ":" _ ) = "[" ++ showL v ++ "]"
	show ( Complex n vs )	= "(" ++ n ++ " " ++ unwords ( map show vs ) ++ ")"
	show ( Error msg )	= "Error: " ++ msg

showL :: Value -> String
showL ( Complex ":" [ v, Empty ] )	= show v
showL ( Complex ":" [ v, c ] )	= show v ++ "," ++ showL c

showValue :: Value -> IO ()
showValue ( IOAction act ) = do
	v <- act
	case v of
		Nil	-> return ()
		_	-> print v
showValue v = print v
