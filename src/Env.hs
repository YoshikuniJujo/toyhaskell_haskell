module Env (
	Env,
	initialize,
	setVars,
	setPat,
	setPats,
	getVal,
	getVars,
	mapEnv,
	Var(V),
) where

import Data.Maybe (listToMaybe)
import Control.Arrow (first)

data Var = V String Int deriving Eq
instance Show Var where show (V x n) = x ++ "~" ++ show n

data Env p v = Env [([Var], p, v)] deriving Show

empty :: Env p v
empty = Env []

initialize :: (String -> p) -> [(String, v)] -> Env p v
initialize p = setVars (p . \(V x _) -> x) empty . map (first $ flip V 0)

setVar :: (Var -> p) -> Env p v -> Var -> v -> Env p v
setVar vp (Env env) var val = Env $ ([var], vp var, val) : env

setVars :: (Var -> p) -> Env p v -> [(Var, v)] -> Env p v
setVars vp = foldl $ uncurry . setVar vp

setPat :: (p -> [Var]) -> Env p v -> p -> v -> Env p v
setPat pv (Env env) pat val = Env $ (pv pat, pat, val) : env

setPats :: (p -> [Var]) -> Env p v -> [(p, v)] -> Env p v
setPats pv = foldl $ uncurry . setPat pv

type Fun a	= a -> a
type Match v p	= v -> p -> Maybe [(Var, v)]

getVal :: Match v p -> Fun v -> Env p v -> Var -> Maybe v
getVal match eval (Env env) var = do
	(_, pat, val) <- listToMaybe $ filter ((var `elem`) . one) env
	match (eval val) pat >>= lookup var

getVars :: Env p v -> [String]
getVars (Env env) = (map (\(V x _) -> x) . one) `concatMap` env

mapEnv :: Fun Var -> Fun p -> Fun v -> Env p v -> Env p v
mapEnv fvar fpat fval (Env env) = Env $
	(\(var, pat, val) -> (map fvar var, fpat pat, fval val)) `map` env

one :: (a, b, c) -> a
one (x, _, _) = x
