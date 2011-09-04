module Main where

some = let 1 : x : 3 : [] = 1 : 2 : 3 : [] in x

x = 9

other = let z = 3 in ( \y -> x + y ) 4

add x y = x + y

ret = add ( let x = 5 in x + other ) some
