module BFRep where

import Data.Maybe

data BFChar = BPlus | BMinus | BLeft | BRight | BIn | BOut | BLBrack | BRBrack | BEnd deriving (Eq)

instance Show BFChar where
    show BPlus   = "+"
    show BMinus  = "-"
    show BLeft   = "<"
    show BRight  = ">"
    show BIn     = ","
    show BOut    = "."
    show BLBrack = "["
    show BRBrack = "]"
    show BEnd    = "!"

loadBF :: IO [BFChar]  -- TODO: Take input filename
loadBF = do
    prog <- readFile "demo.b"
    return (mapMaybe toBFChar prog)

toBFChars :: String -> [BFChar]
toBFChars = mapMaybe toBFChar

toBFChar :: Char -> Maybe BFChar
toBFChar c = case c of
    '+' -> Just BPlus
    '-' -> Just BMinus
    '<' -> Just BLeft
    '>' -> Just BRight
    ',' -> Just BIn
    '.' -> Just BOut
    '[' -> Just BLBrack
    ']' -> Just BRBrack
    _   -> Nothing