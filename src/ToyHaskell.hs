{-# LANGUAGE TupleSections #-}

module ToyHaskell ( Env, primitives, load, eval, evalP ) where

import Primitives ( primitives )
import Eval ( toyEval )
import Alpha ( alpha, alphaEnv )
import Parser ( toyParse, toyParseModule )
import Value (
	Value( Nil, Empty, Integer, Char, Comp, IOAction, Let, Module ),
	Env, setPats, getVars )

class ToyValue a where
	fromToyValue :: Value -> a

instance ToyValue () where
	fromToyValue Nil	= ()
	fromToyValue _		= error "Not ()"

instance ToyValue Bool where
	fromToyValue ( Comp "True" [ ] )	= True
	fromToyValue ( Comp "False" [ ] )	= False
	fromToyValue _				= error "Not Bool"

instance ToyValue Integer where
	fromToyValue ( Integer i )	= i
	fromToyValue _			= error "Not Integer"

instance ToyValue Char where
	fromToyValue ( Char c )	= c
	fromToyValue _		= error "Not Char"

instance ToyValue a => ToyValue [ a ] where
	fromToyValue lst	= map fromToyValue $ fromToyList lst
		where
		fromToyList Empty			= [ ]
		fromToyList ( Comp ":" [ h, t ] )	= h : fromToyList t
		fromToyList _				= error "Not list"

instance ToyValue a => ToyValue ( IO a ) where
	fromToyValue ( IOAction a )	= fromToyValue `fmap` a
	fromToyValue _			= error "Not IO"

evalV :: Env -> String -> Value
evalV env = toyEval env . alpha ( getVars env ) . toyParse

evalP :: Env -> String -> IO ( String, Env )
evalP env src = case evalV env src of
	Let ps		-> return ( "", setPats ps $ alphaEnv ( map fst ps ) env )
	IOAction act	-> ( ( , env ) . showVal ) `fmap` act
	val		-> return ( showVal val, env )
	where
	showVal Nil	= ""
	showVal v	= show v ++ "\n"

eval :: ToyValue a => Env -> String -> a
eval = (.) fromToyValue . evalV

load :: Env -> String -> Env
load e src = case toyEval e $ alpha ( getVars e ) $ toyParseModule src of
	Module ps	-> setPats ps e
	nm		-> error $ "never occur" ++ show nm
