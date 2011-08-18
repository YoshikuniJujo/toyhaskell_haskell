module MainTools (
	mainGen
) where

import Prelude hiding ( lex )
import Parser
import Eval
import Value
import Interact
import Data.List
import Data.Char
import Control.Monad
import Control.Arrow

mainGen :: [ String ] -> IO ()
mainGen args = do
	let ( expr, fns ) = getExpArgs args
	env0 <-  foldM ( flip ( runCmd .  ( "load " ++ ) ) ) initEnv fns
	case expr of
		Nothing -> runLoop "testLexer" env0 $ \env input -> case input of
			':' : cmd	-> runCmd cmd env
			_		-> case eval env $ toyParse input of
				Let ps	-> return $ map ( first patVar ) ps ++ env
				ret	-> showValue ret >> return env
		Just e -> showValue $ eval env0 $ toyParse e

runCmd :: String -> Env -> IO Env
runCmd cmd env
	| "load" `isPrefixOf` cmd	= do
		let fn = dropWhile isSpace $ drop 4 cmd
		cnt <- readFile fn
		case eval env $ toyParse ( "let " ++ cnt ) of
			Let ps	-> return $ map ( first patVar ) ps ++ env
			bad	-> error $ show bad
	| otherwise			= return env

getExpArgs :: [ String ] -> ( Maybe String, [ String ] )
getExpArgs ( "-e" : expr : rest ) = ( Just expr, rest )
getExpArgs rest = ( Nothing, rest )
