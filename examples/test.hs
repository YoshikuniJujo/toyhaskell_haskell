hello = "Hello, world!\n"
bye = "Good-bye, world"

fac n = if n == 0 then 1 else n * fac ( n - 1 )

fac2 n = ( case n of 0 -> 1; _ -> n * fac ( n - 1 ))

myPutStr str = ( case str of c : cs -> putChar c >> myPutStr cs; [] -> return () )

main = myPutStr hello
