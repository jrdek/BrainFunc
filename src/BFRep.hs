module BFRep where
    -- TODO: set what functions this exports

import Data.Maybe
import Debug.Trace

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

-- Now let's do a different approach which should condense the programs being run

-- oh no, BFTs
data BFToken = BtokAdd Int | BtokMov Int | BtokIn | BtokOut | BtokWhile [BFToken] | BtokEnd deriving (Eq)

instance Show BFToken where
    show (BtokAdd i) = "add " ++ show i
    show (BtokMov i) = "mov " ++ show i
    show (BtokIn)    = "input"
    show (BtokOut)   = "print"
    show (BtokWhile toks) = "while {" ++ show toks ++ "}"
    show (BtokEnd)   = "end"

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead xs = Just (head xs)

safeTail :: [a] -> [a]
safeTail [] = []
safeTail xs = tail xs

-- fun idea about bftokens: maybe we can change these back to bchars
-- program synthesis as a future direction???

-- toBFTokensH prog = (remainingProg, token)
-- NOTE: seenToks is reversed! 
toBFToken :: String -> (String, Maybe BFToken)
toBFToken ""       = ("", Just BtokEnd)
toBFToken ('[':cs) =  (fst b, Just $ BtokWhile (snd b))
    where b = toBFTokensH cs
toBFToken ('+':cs) = (cs, Just $ BtokAdd 1)
toBFToken ('-':cs) = (cs, Just $ BtokAdd (-1))
toBFToken ('>':cs) = (cs, Just $ BtokMov 1)
toBFToken ('<':cs) = (cs, Just $ BtokMov (-1))
toBFToken ('.':cs) = (cs, Just BtokOut)
toBFToken (',':cs) = (cs, Just BtokIn)
-- toBFToken (']':cs) = (cs, Just BtokEnd)
toBFToken (_:cs)   = (cs, Nothing)

toBFTokensH :: String -> (String, [BFToken])
toBFTokensH ""       = ("", [])
toBFTokensH (']':cs) = (cs, [])
toBFTokensH str      = (rcs, alltoks)
    where 
     rcs = fst restParsed
     alltoks = case thisToken of
         BtokEnd -> snd restParsed
         tok     -> tok:(snd restParsed)
     restParsed = toBFTokensH $ fst getThisToken
     thisToken = case snd getThisToken of
         Just j -> j
         Nothing -> BtokEnd
     getThisToken = toBFToken str

toBFTokens :: String -> [BFToken]
toBFTokens s = snd $ toBFTokensH s

collapseTokensList :: [BFToken] -> [BFToken]
collapseTokensList toks = case toks of 
    (BtokAdd n) : (BtokAdd m) : rest -> collapseTokensList $ if n+m /= 0 then (BtokAdd $ n + m):rest else rest
    (BtokMov n) : (BtokMov m) : rest -> collapseTokensList $ if n+m /= 0 then (BtokMov $ n + m):rest else rest
    (BtokWhile w) : rest -> BtokWhile (collapseTokensList w) : (collapseTokensList rest)
    tok : rest -> tok:(collapseTokensList rest)
    [] -> []

checkBrackets :: String -> String
checkBrackets s = if checkBracketsH 0 s == 0 then s else error "This should be unreachable..."

checkBracketsH :: Int -> String -> Int
checkBracketsH n s
    | n < 0                 = error "Mismatched brackets"
    | (s == "") && (n /= 0) = error "Mismatched brackets"
    | s == ""               = 0  -- hooray!
    | (s !! 0 == '[')       = checkBracketsH (n + 1) (tail s)
    | (s !! 0 == ']')       = checkBracketsH (n - 1) (tail s)
    | otherwise             = checkBracketsH n (tail s)


makeCollapsedTokens :: String -> [BFToken]
makeCollapsedTokens = collapseTokensList . toBFTokens . checkBrackets

-- >>> let prog = ""
-- >>> makeCollapsedTokens prog
-- [add 1,while {[while {[mov 1,while {[add 1,mov 1,while {[add -1,mov -1,add 3,mov 1]},mov -1,print]}]}]}]
--


