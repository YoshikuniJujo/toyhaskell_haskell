module Main where

import System.Environment ( getArgs )
import MainTools ( mainGen )

main :: IO ()
main = getArgs >>= mainGen . ( "-e" : ) . ( "main" : ) . ( : [] ) . head
