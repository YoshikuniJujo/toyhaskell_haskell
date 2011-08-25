module Main where

-- import Prelude hiding ( putStr, putStrLn )

some = let { 1 : x : 3 : [] = 1 : 2 : 3 : [] } in x;

hello = "Hello, world!\n";
bye = "Good-bye, world";

fac n = if n == 0 then 1 else n * fac ( n - 1 );

-- fac2 n = case n of { 0 -> 1; _ -> n * fac ( n - 1 ) };
fac2 n = case n of
	0 -> 1
	_ -> n * fac ( n - 1 )
;

fac3 = \n -> case n of { 0 -> 1; _ -> n * fac ( n - 1 ) };

putStr str = case str of { c : cs -> putChar c >> putStr cs; [] -> return () };
putStrLn str = putStr str >> putChar '\n';

main = putStrLn "Hello, World!" >> return some ;
