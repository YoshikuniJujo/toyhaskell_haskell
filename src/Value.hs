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
	Integer Integer |
	String String |
	Bool Bool |
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
	show ( Integer n )	= show n
	show ( String s )	= show s
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
	show ( Error msg )	= "Error: " ++ msg

showValue :: Value -> IO ()
showValue ( IOAction act ) = do
	v <- act
	case v of
		Nil	-> return ()
		_	-> print v
showValue v = print v
