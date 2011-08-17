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
	Identifier String |
	Function ( Value -> Value ) |
	IOAction ( IO Value ) |
	Apply Value Value |
	Lambda Env [ String ] Value

instance Show Value where
	show Nil = "()"
	show ( Integer n ) = show n
	show ( String s ) = show s
	show ( Identifier i ) = i
	show ( Function _ ) = "<function>"
	show ( IOAction _ ) = "<IO>"
	show ( Apply f a ) = "(" ++ show f ++ " " ++ show a ++ ")"
	show ( Lambda _ _ _ ) = "<closure>"

showValue :: Value -> IO ()
showValue ( IOAction act ) = do
	v <- act
	case v of
		Nil	-> return ()
		_	-> print v
showValue v = print v
