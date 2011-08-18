module Main where

import Parser

main :: IO ()
main = interact $ eraseImport . addSemi 0
