module Fixity (fixity, initFix) where

import Value

data Assoc	= Left | Right | None
data Fixity	= Infix Assoc Int

initFix :: [(String, Fixity)]
initFix = []

fixity :: [(String, Fixity)] -> Infix -> Value
fixity _ (Value v)	= v
fixity fl (Op op x y)	= App (App (Var op 0) $ fixity fl x) $ fixity fl y
{-
fixity fl (Con con vs)	= Con con $ map (fixity fl) vs
fixity _ (Op op x y)	= App (App (Var op 0) x) y
fixity _ v		= v
-}
