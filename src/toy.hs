module Main where

import MainTools ( mainGen )
import System.Environment ( getArgs )
import Control.Arrow ( second )

main :: IO ()
main = getArgs >>= uncurry mainGen . second tail . span ( /= "--" ) >>= putStr
