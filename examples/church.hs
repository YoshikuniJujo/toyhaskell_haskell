module Main where

add1 x = x + 1

one f x = f x
two f x = f ( f x )

eight f x = f ( f ( f ( f ( f ( f ( f ( f x ) ) ) ) ) ) )

toInt c = c add1 0

succ n f x = f ( n f x )
plus m n f x = m f ( n f x )
