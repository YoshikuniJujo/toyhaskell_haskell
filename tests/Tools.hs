{-# LANGUAGE PatternGuards #-}

module Tools (mapFilter) where

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter _ []		= []
mapFilter f (x : xs)
	| Just y <- f x	= y : mapFilter f xs
	| otherwise	= mapFilter f xs
