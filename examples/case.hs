module Main where

three = f [ 3 ]

f = \x -> case x of x : _ -> x
