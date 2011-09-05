module ToyHaskell (
	evalV, eval, load, initEnv, toyEval, Env, setPats, showValue,
	Value( Let ) ) where

import Primitives
import Eval
import Alpha
import Parser
import Value

class ToyValue a where
	fromToyValue :: Value -> a

instance ToyValue Integer where
	fromToyValue ( Integer i )	= i
	fromToyValue _			= error "Not Integer"

instance ToyValue () where
	fromToyValue Nil	= ()
	fromToyValue _		= error "Not ()"

instance ToyValue a => ToyValue ( IO a ) where
	fromToyValue ( IOAction a )	= fromToyValue `fmap` a
	fromToyValue _			= error "Not IO"

evalV :: Env -> String -> Value
evalV env = toyEval env . alpha ( getVars env ) . toyParse

eval :: ToyValue a => Env -> String -> a
eval = (.) fromToyValue . evalV

load :: Env -> String -> Env
load env src = case toyEval env $ alpha [ ] $ toyParseModule src of
	Module ps	-> setPats ps env
	_		-> error "never occur"
