{-# LANGUAGE TupleSections #-}

module ToyHaskell ( Env, initEnv, load, eval, evalP ) where

import Primitives ( initEnv )
import Eval ( toyEval )
import Alpha ( toyAlpha, alphaEnvsP )
import Parser ( toyParse, toyParseModule )
import Value (
	Value( Nil, Empty, Integer, Char, Complex, IOAction, Let, Module ),
	Env, setPats, getVars )

import Control.Arrow ( second )

class ToyValue a where
	fromToyValue :: Value -> a

instance ToyValue () where
	fromToyValue Nil	= ()
	fromToyValue _		= error "Not ()"

instance ToyValue Bool where
	fromToyValue ( Complex "True" [ ] )	= True
	fromToyValue ( Complex "False" [ ] )	= False
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
		fromToyList ( Complex ":" [ h, t ] )	= h : fromToyList t
		fromToyList _				= error "Not list"

instance ToyValue a => ToyValue ( IO a ) where
	fromToyValue ( IOAction a )	= fromToyValue `fmap` a
	fromToyValue _			= error "Not IO"

evalV :: Env -> String -> Value
evalV env = toyEval env . toyAlpha ( getVars env ) . toyParse

evalP :: Env -> String -> IO ( String, Env )
evalP env src = case evalV env src of
	Let ps		-> return ( "", setPats ps $ alphaEnvsP ( map fst ps ) env )
--		let newEnv = setPats ( second ( toyEval newEnv )`map` ps ) env
--			in newEnv )
--		setPats ( second ( toyEval env ) `map` ps ) env )
	IOAction act	-> ( ( , env ) . showVal ) `fmap` act
	val		-> return ( showVal val, env )
	where
	showVal Nil	= ""
	showVal v	= show v ++ "\n"

eval :: ToyValue a => Env -> String -> a
eval = (.) fromToyValue . evalV

load :: Env -> String -> Env
load e src = case toyEval e $ toyAlpha ( getVars e ) $ toyParseModule src of
	Module ps	-> setPats ps e
	_		-> error "never occur"
