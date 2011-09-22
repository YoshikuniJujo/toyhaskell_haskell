module Eval ( toyEval ) where

import Value ( Value(
	Var, Comp, App, Fun, Lambda, Closure, Case, Letin, Let, Module, Error ),
	Pattern, match, patVars, Env, setVars, setPat, setPats, getVal, Var( V ) )
import Data.Maybe ( fromMaybe )

--------------------------------------------------------------------------------

toyEval :: Env -> Value -> Value
toyEval env val = case noVars env val of
	[ ]	-> eval env val
	vars	-> Error $ unlines ( map errMsg vars )
	where errMsg var = "\tNot in scope: `" ++ var ++ "'"

get :: Env -> Var -> Maybe Value
get env = getVal ( eval env ) env

noVars :: Env -> Value -> [ String ]
noVars env ( Var v n )		= maybe [ v ] ( noVars env ) $ env `get` V v n
noVars env ( App f a )		= noVars env f ++ noVars env a
noVars env ( Lambda ps ex )	= ps `filterVars` noVars env ex
noVars env ( Case key alts )	= noVars env key ++ nvc `concatMap` alts
	where nvc ( pat, ex ) = [ pat ] `filterVars` noVars env ex
noVars env ( Letin defs ex )	=
	map fst defs `filterVars` noVars env ex ++ noVars env ( Let defs )
noVars env ( Let defs )		=
	map fst defs `filterVars` ( noVars env . snd ) `concatMap` defs
noVars env ( Module defs )	= noVars env ( Let defs )
noVars _ _			= [ ]

infixl	8 `filterVars`
filterVars :: [ Pattern ] -> [ String ] -> [ String ]
filterVars pats = filter ( `notElem` patVars `concatMap` pats )

eval :: Env -> Value -> Value
eval env ( Var v n )		= eval env $ fromMaybe ( noVar v n ) $ env `get` V v n
eval env ( Comp con mems )	= Comp con $ eval env `map` mems
eval env ( App f a )		= case eval env f of
	Fun fun			-> fun $ eval env a
	Closure ce [ p ] e	-> eval ( setPat ce p ( eval env a ) ) e
	Closure ce ( p : ps ) e	-> Closure ( setPat ce p ( eval env a ) ) ps e
	err@( Error _ )		-> err
	_			-> notFunction f
eval env ( Lambda ps ex )	= Closure env ps ex
eval env ( Case key alts )	= ec alts
	where
	ec [ ]			= nonExhaustive
	ec ( ( pat, ex ) : r )	= let k = eval env key in
		maybe ( ec r ) ( flip eval ex . setVars env ) $ match k pat
eval env ( Letin defs ex )	= eval ( setPats env defs ) ex
eval _ v			= v


--------------------------------------------------------------------------------

noVar :: String -> Int -> Value
noVar var n = Error $ "Not in scope: `" ++ var ++ case n of
	0 -> "~" ++ show n  ++ "'"
	_ -> "'"

notFunction :: Value -> Value
notFunction nf = Error $ "Not Function: " ++ show nf

nonExhaustive :: Value
nonExhaustive = Error "Non-exhaustive patterns in case"
