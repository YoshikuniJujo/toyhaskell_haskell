module Eval (toyEval) where

import Value (Value(
	Var, Comp, App, Fun, Lambda, Closure, Case, Letin, Let, Module, Err),
	Pattern, match, patVars,
	Env, setVars, setPat, setPats, getVal, Var(V))
import Data.Maybe (fromMaybe)

--------------------------------------------------------------------------------

toyEval :: Env -> Value -> Value
toyEval env val = case noVars env val of
	[]	-> eval env val
	vars	-> Err $ unlines (map errMsg vars)
	where errMsg var = "\tNot in scope: `" ++ var ++ "'"

get :: Env -> Var -> Maybe Value
get env = getVal (eval env) env

noVars :: Env -> Value -> [String]
noVars env (Var x n)		= maybe [x] (noVars env) $ env `get` V x n
noVars env (Comp _ mems)	= noVars env `concatMap` mems
noVars env (App f a)		= noVars env f ++ noVars env a
noVars env (Lambda ps ex)	= ps `filterVars` noVars env ex
noVars env (Case key alts)	= noVars env key ++ nvc `concatMap` alts
	where nvc (test, ex) = [test] `filterVars` noVars env ex
noVars env (Letin defs ex)	=
	map fst defs `filterVars` noVars env ex ++ noVars env (Module defs)
noVars env (Module defs)	=
	map fst defs `filterVars` (noVars env . snd) `concatMap` defs
noVars env (Let defs)		= noVars env (Module defs)
noVars _ _			= []

infixl	8 `filterVars`
filterVars :: [Pattern] -> [String] -> [String]
filterVars pats = filter (`notElem` patVars `concatMap` pats)

eval :: Env -> Value -> Value
eval env (Var x n)		= eval env $ fromMaybe (noVar x n) $ env `get` V x n
eval env (Comp con mems)	= Comp con $ eval env `map` mems
eval env (App f a)		= case eval env f of
	Fun fun			-> fun $ eval env a
	Closure ce [p] e	-> eval (setPat ce p (eval env a)) e
	Closure ce (p : ps) e	-> Closure (setPat ce p (eval env a)) ps e
	err@(Err _)		-> err
	_			-> notFunction f
eval env (Lambda ps ex)	= Closure env ps ex
eval env (Case key alts)	= ec alts
	where
	ec []			= nonExhaustive
	ec ((test, ex) : r)	= let k = eval env key in
		maybe (ec r) (flip eval ex . setVars env) $ match k test
eval env (Letin defs ex)	= eval (setPats env defs) ex
eval _ v			= v


--------------------------------------------------------------------------------

noVar :: String -> Int -> Value
noVar x n = Err $ "Not in scope: `" ++ x ++ case n of
	0 -> "~" ++ show n  ++ "'"
	_ -> "'"

notFunction :: Value -> Value
notFunction nf = Err $ "Not Function: " ++ show nf

nonExhaustive :: Value
nonExhaustive = Err "Non-exhaustive patterns in case"
