module Main where

i x = x
k x y = x
s x y z = ( x z ) ( y z )

--

twice = s s ( k i ) i

f g h y = g ( h i y )

two = s ( k twice ) f
