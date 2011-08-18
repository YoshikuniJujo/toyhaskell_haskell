module Main where

import Parser
import System.Environment

main :: IO ()
main = do
--	cnt <- fmap head getArgs >>= readFile
	interact $ eraseImport . addSemi 0
