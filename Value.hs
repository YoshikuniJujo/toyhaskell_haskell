module Value (
	Env,
	Value( .. ),
	showValue
) where

type Env = [ ( String, Value ) ]

data Value =
	Nil |
	Integer Integer |
	String String |
	Bool Bool |
	Identifier String |
	Function ( Value -> Value ) |
	IOAction ( IO Value ) |
	Apply Value Value |
	Lambda Env [ String ] Value |
	Letin [ ( String, Value ) ] Value |
	Let [ ( String, Value ) ] |
	If Value Value Value |
	Error String

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
	show ( Error msg )	= "Error: " ++ msg

showValue :: Value -> IO ()
showValue ( IOAction act ) = do
	v <- act
	case v of
		Nil	-> return ()
		_	-> print v
showValue v = print v
