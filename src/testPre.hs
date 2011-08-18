module Main where

import Parser
import System.Environment

main :: IO ()
main = do
	cnt <- fmap head getArgs >>= readFile
	putStr $ eraseImport $ addSemi 0 cnt
