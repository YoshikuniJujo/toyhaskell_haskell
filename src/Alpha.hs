{-# LANGUAGE TupleSections #-}

module Alpha (alpha, alphaEnv) where

import Value (
	Value(Var, Con, App, Lambda, Case, Letin, Module, Let),
	Pattern(PatVar, PatCon), patVars, Env, mapEnv, Var(V))
import Data.List (intersect, union)
import Control.Arrow (second, (***), (&&&))

--------------------------------------------------------------------------------

alphaEnv :: [Pattern] -> Env -> Env
alphaEnv = flip (foldr ae)  . (patVars `concatMap`)
	where ae v = mapEnv (succV v) (succV v) (succV v)

alpha :: [String] -> Value -> Value
alpha pre (Con con mems)	= Con con $ alpha pre `map` mems
alpha pre (App f a)		= App (alpha pre f) $ alpha pre a
alpha pre (Lambda ps e)	= let (dups, np) = pre `iu` ps in
	Lambda (map (succVs dups) ps) $ alpha np $ succVs dups e
alpha pre (Case key alts)	= Case (alpha pre key) $ ac `map` alts
	where
	ac alt@(test, _) = let (dups, np) = pre `iu` [test] in
		second (alpha np) $ succVs dups alt
alpha pre (Letin defs ex)	= let (dups, np) = pre `iu` map fst defs in
	Letin (alphaDefs pre defs) $ alpha np $ succVs dups ex
alpha pre (Module defs)	= Module $ alphaDefs pre defs
alpha pre (Let defs)		= Let $ second (alpha pre) `map` defs
alpha _ val			= val

alphaDefs :: [String] -> [(Pattern, Value)] -> [(Pattern, Value)]
alphaDefs pre defs		= let (dups, np) = pre `iu` map fst defs in
	(second (alpha np) . succVs dups) `map` defs

iu :: [String] -> [Pattern] -> ([String], [String])
iu = curry $
	(uncurry intersect &&& uncurry union) . second (patVars `concatMap`)

succVs :: Alpha sv => [String] -> sv -> sv
succVs = flip $ foldr succV

class Alpha sv where
	succV :: String -> sv -> sv

instance Alpha Var where
	succV v v1@(V x n)
		| v == x	= V x $ n + 1
		| otherwise	= v1

instance Alpha Pattern where
	succV v (PatVar x n)
		| v == x	= PatVar x $ n + 1
	succV v (PatCon c ms)	= PatCon c $ succV v `map` ms
	succV _ pat		= pat

instance Alpha Value where
	succV v (Var x n)
		| v == x	= Var x $ n + 1
	succV v (Con c ms)	= Con c $ succV v `map` ms
	succV v (App f a)	= App (succV v f) $ succV v a
	succV v (Lambda ps e)	= Lambda (succV v `map` ps) $ succV v e
	succV v (Case k alts)	= Case (succV v k) (succV v `map` alts)
	succV v (Letin ds e)	= Letin (succV v `map` ds) $ succV v e
	succV _ val		= val

instance (Alpha a, Alpha b) => Alpha (a, b) where
	succV vars = succV vars *** succV vars
