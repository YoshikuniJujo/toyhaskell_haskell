module Main where

import System.Environment
import MainTools

main :: IO ()
main = getArgs >>= mainGen
