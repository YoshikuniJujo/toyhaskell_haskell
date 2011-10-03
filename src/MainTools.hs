{-# LANGUAGE TupleSections #-}

module MainTools (main) where

import ToyHaskell (Env, load, evalP, primitives)

import System.IO (hFlush, stdout)
import System.Console.GetOpt (getOpt, ArgOrder(..), OptDescr(..), ArgDescr(..))
import Control.Monad (foldM)
import Control.Monad.Tools (doWhile)

--------------------------------------------------------------------------------

main :: [String] -> [String] -> IO String
main args _ = do
	(expr, fns) <- readArgs args
	env0 <- foldM (\env -> (load env `fmap`) . readFile) primitives fns
	(flip . flip maybe) (fmap fst . evalP env0) expr $ do
		runLoop "toyhaskell" env0 $ \env inp -> case inp of
			':' : str	-> let	cmd : args' = words str in
						runCmd env cmd args'
			_		-> do	(ret, env') <- evalP env inp
						putStr ret
						return env'

runLoop :: String -> a -> (a -> String -> IO a) -> IO String
runLoop name stat0 act = do
	_ <- doWhile stat0 $ \stat -> do
		input <- putStr (name ++ "> ") >> hFlush stdout >> getLine
		if input `elem` [":quit", ":q"]
			then return (stat, False)
			else (, True) `fmap` act stat input
	return $ "Leaving " ++ name ++ ".\n"

runCmd :: Env -> String -> [String] -> IO Env
runCmd env "load" args	= load env `fmap` readFile (head args)
runCmd env cmd _	= do	putStrLn ("unknown command ':" ++ cmd  ++ "'")
				return env

data Option = Expr String

options :: [OptDescr Option]
options = [
	Option "e" [] (ReqArg Expr "haskell expression") "run expression"
 ]

readArgs :: [String] -> IO (Maybe String, [FilePath])
readArgs args = let
	(opts, fns, errs)	= getOpt RequireOrder options args
	expr			= fromOps opts in do
	mapM_ putStr errs
	return (expr, fns)
	where
	fromOps []		= Nothing
	fromOps (Expr e : _)	= Just e
