{-# LANGUAGE TupleSections #-}

module ToyHaskell (Env, primitives, load, eval, evalP) where

import Primitives (primitives)
import Eval (toyEval)
import Alpha (alpha, alphaEnv)
import Parser (parse, parseModule)
import Value (
	Value(Nil, Empty, Integer, Char, IOAction, Con, Module, Let),
	Env, setPats, getVars)

class ToyValue a where
	fromToyValue :: Value -> a

instance ToyValue () where
	fromToyValue Nil	= ()
	fromToyValue _		= error "Not ()"

instance ToyValue Bool where
	fromToyValue (Con "True" [])	= True
	fromToyValue (Con "False" [])	= False
	fromToyValue _			= error "Not Bool"

instance ToyValue Integer where
	fromToyValue (Integer i)	= i
	fromToyValue _			= error "Not Integer"

instance ToyValue Char where
	fromToyValue (Char c)	= c
	fromToyValue _		= error "Not Char"

instance ToyValue a => ToyValue [a] where
	fromToyValue lst = map fromToyValue $ fromToyList lst
		where
		fromToyList Empty		= []
		fromToyList (Con ":" [h, t])	= h : fromToyList t
		fromToyList _			= error "Not list"

instance ToyValue a => ToyValue (IO a) where
	fromToyValue (IOAction a)	= fromToyValue `fmap` a
	fromToyValue _			= error "Not IO"

evalV :: Env -> String -> Value
evalV env = toyEval env . alpha (getVars env) . parse

evalP :: Env -> String -> IO (String, Env)
evalP env src = case evalV env src of
	Let defs	->
		return ("", setPats (alphaEnv (map fst defs) env) defs)
	IOAction act	-> ((, env) . showVal) `fmap` act
	val		-> return (showVal val, env)
	where
	showVal Nil	= ""
	showVal v	= show v ++ "\n"

eval :: ToyValue a => Env -> String -> a
eval = (.) fromToyValue . evalV

load :: Env -> String -> Env
load env src = case toyEval env $ alpha (getVars env) $ parseModule src of
	Module ps	-> setPats env ps
	nm		-> error $ "not module: " ++ show nm
