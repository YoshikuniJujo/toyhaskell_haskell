module Main where

empty c = c ( \x -> x )
zero c = c ( \f -> f s k )
one c ll = ll ( \l rr -> rr ( \r -> c ( l r ) ) )

k x y = x
s x y z = x z ( y z )

e = empty
i = one
o = zero

add x y = x + y
add8 = add 8

ii = e i o o

interrogate f = f ii ii ii k

output = k ( k ( k ( k ( k ( k ii ) ) ) ) )
