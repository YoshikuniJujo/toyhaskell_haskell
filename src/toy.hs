module Main where

import qualified MainTools as T (main)
import System.Environment (getArgs)
import Control.Arrow (second)

main :: IO ()
main = getArgs >>= uncurry T.main . second tail . span (/= "--") >>= putStr
