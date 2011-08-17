module Main where

import Text.RegexPR
import System.Directory
import Data.List
import Data.Tree
import System.Environment

main :: IO ()
main = do
	[ n, dir ] <- getArgs
	files <- fmap filterSource $ getDirectoryContents dir
	dependList <- mapM ( depend dir files ) files
	mapM_ ( putStr . showTree [ ] . nubTree ) $
		let xs = mergeTree ( read n ) $ map makeTree dependList in xs

showTree :: [ Bool ] -> Tree String -> String
showTree n ( Node x ns ) =
	makePre ( reverse n ) ++ x ++ "\n" ++
		( concatMap ( showTree ( True : n ) ) ( init ns ) ++
		maybe "" ( showTree ( False : n ) ) ( last ns ) )
	where
	init [ ] = [ ]
	init [ x ] = [ ]
	init ( x : xs ) = x : init xs
	last [ ] = Nothing
	last [ x ] = Just x
	last ( _ : xs ) = last xs
	makePre [ ] = ""
	makePre [ _ ] = "  + "
	makePre ( True  : rest ) = "  | " ++ makePre rest
	makePre ( False : rest ) = "    " ++ makePre rest

nubTree :: Eq a => Tree a -> Tree a
nubTree ( Node x ns ) = Node x $ nub $ map nubTree ns

makeTree :: Eq a => ( a, [ a ] ) -> Tree a
makeTree ( x, xs ) = Node x $ map ( flip Node [ ] ) xs

mergeTree :: Eq a => Int -> [ Tree a ] -> [ Tree a ]
-- mergeTree [ t ] = [ t ]
mergeTree n ts
	| n == length ts = ts
	| otherwise = mergeTree n $
		map fst $ filter snd $ map ( `addTree_` ts ) ts

addTree_ :: Eq a => Tree a -> [ Tree a ] -> ( Tree a, Bool )
addTree_ t@( Node x _ ) ts = addTree t $ filter ( ( /= x ) . rootLabel ) ts

addTree :: Eq a => Tree a -> [ Tree a ] -> ( Tree a, Bool )
addTree ( Node x ns ) ts = case filter ( ( == x ) . rootLabel ) ts of
	[ ]	-> ( Node x $ map fst rets, any snd rets )
	t : _	-> ( t, True )
	where
	rets = map ( `addTree` ts ) ns

depend :: FilePath -> [ String ] -> String -> IO ( String, [ String ] )
depend dir fps fp = do
	cnt <- readFile $ dir ++ "/" ++ fp ++ ".hs"
	return ( fp, map ( !! 1 ) $ ggetbrsRegexPR ( mkReg fps ) cnt )

filterSource :: [ FilePath ] -> [ FilePath ]
filterSource =
	map ( initN 3 ) . filter ( isSuffixOf ".hs" ) .
	filter ( not . isPrefixOf "." )

mkReg :: [ FilePath ] -> String
mkReg fps = "^import\\s+(?:qualified\\s+)?(" ++ intercalate "|" fps ++ ")"

initN :: Int -> [ a ] -> [ a ]
initN n = ( !! n ) . iterate init
