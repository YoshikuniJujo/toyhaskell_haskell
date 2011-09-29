module Main where

main :: IO ()
main = interact countLine

countLine :: String -> String
countLine = (++ "\n") . show . length . filter (not . null) . lines
