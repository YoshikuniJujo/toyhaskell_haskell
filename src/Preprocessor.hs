module Preprocessor (
	prep
) where

import Data.Char
import Data.List

prep :: String -> String
prep = eraseImport . addSemi 0

addSemi :: Int -> String -> String
addSemi _ "" = ""
addSemi n ( c1 : 'i' : 'n' : c2 : rest )
	| isSpace c1 && isSpace c2 = ' ' : ';' : 'i' : 'n' : ' ' : addSemi n rest
addSemi n0 ( '\n' : rest ) = let n1 = length $ takeWhile ( == '\t' ) rest in
	if n0 >= n1
		then ' ' : ';' : '\n' : addSemi n1 rest
		else '\n' : addSemi n1 rest
addSemi n ( c : cs ) = c : addSemi n cs

eraseImport :: String -> String
eraseImport = unlines . filter ( not . ( "import " `isPrefixOf` ) ) . lines
