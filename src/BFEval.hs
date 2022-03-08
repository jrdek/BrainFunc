module BFEval where

import BFRep
import Control.Monad.State
import Data.Array
import Data.Char
import Data.Map (Map, lookup, findWithDefault, insert, empty)
import Text.Read

{-
TODO: Rewrite this description

Evaluation requires
    1. a snapshot of the tape
    2. the program
    3. PC and tape head location
    4. the current instruction

Furthermore, we need a stack of bracket locations if where
want the bracket handling to not be absolutely awful.

TODO: Update! We're using lookup tables instead of a stack
-}

type Stack = [Int]

data BFMachine = BFMachine {
    prog :: Array Int BFChar,  -- the program is immutable :)
    cyc  :: Int,
    tape :: Map Int Int,
    pc :: Int,
    wh :: Int,
    brax :: Map Int Int
}

instance Show BFMachine where
    show (BFMachine p y t c w b) =
        ("CYCLE " ++ show y) ++ "\n" ++
        ("Tape: " ++ show t) ++ "\n" ++
        ("PC: " ++ show c) ++ "\n" ++
        ("WH: " ++ show w) ++ "\n"
        

-- kinda bad implementation of matching brackets
makeBracketsMap :: [BFChar] -> Maybe (Map Int Int)
makeBracketsMap cs = makeBracketsMapH cs 0 [] (Just empty)

-- makeBracketsMapH progList ind lbrackStack mapping = newMapping
makeBracketsMapH :: [BFChar] -> Int -> [Int] -> Maybe (Map Int Int) -> Maybe (Map Int Int)
makeBracketsMapH (BRBrack:cs) _ [] _ = Nothing
makeBracketsMapH [] _ (b:bs) _       = Nothing
makeBracketsMapH [] _ _ (Just m)            = Just m
makeBracketsMapH (c:cs) i bx (Just m)   = case c of
    BLBrack -> makeBracketsMapH cs (i+1) (i:bx) (Just m)
    BRBrack -> case bx of 
        b:bs -> makeBracketsMapH cs (i+1) bs $ Just (insert b i (insert i b m))
        -- note that if bx were nil, we'd have caught it already!
    _       -> makeBracketsMapH cs (i+1) (bx) (Just m)

indPair :: [a] -> [(Int, a)]
indPair ls = zip [0..(length ls - 1)] ls

initMachine :: [BFChar] -> BFMachine
initMachine pl = (BFMachine p 0 empty 0 0 $! b) where
    p = array (0, length pl) (indPair pl ++ [(length pl, BEnd)])
    b = case makeBracketsMap pl of
        Just m -> m
        Nothing -> error "Compile error: Imbalanced brackets" 

getCurrCmd :: StateT BFMachine IO BFChar
getCurrCmd = state (\ (BFMachine p y t c w b) -> ((p!c), BFMachine p y t c w b))

tapeWrite :: Int -> Int -> StateT BFMachine IO ()
tapeWrite loc n = state (\ (BFMachine p y t c w b) -> ((), BFMachine p y (insert loc n t) c w b))

tapeRead :: Int -> StateT BFMachine IO Int
tapeRead loc = state (\ (BFMachine p y t c w b) -> (findWithDefault 0 loc t, BFMachine p y t c w b))

writePC :: Int -> StateT BFMachine IO ()
writePC n = state (\ (BFMachine p y t c w b) -> ((), BFMachine p y t n w b))

getPC :: StateT BFMachine IO Int
getPC = state (\ (BFMachine p y t c w b) -> (c, BFMachine p y t c w b))

writeCyc :: Int -> StateT BFMachine IO ()
writeCyc n = state (\ (BFMachine p y t c w b) -> ((), BFMachine p n t c w b))

getCyc :: StateT BFMachine IO Int
getCyc = state (\ (BFMachine p y t c w b) -> (y, BFMachine p y t c w b))

advancePC :: StateT BFMachine IO ()
advancePC = do
    pc <- getPC
    writePC (pc+1)

writeWH :: Int -> StateT BFMachine IO ()
writeWH n = state (\ (BFMachine p y t c w b) -> ((), BFMachine p y t c n b))

getWH :: StateT BFMachine IO Int
getWH = state (\ (BFMachine p y t c w b) -> (w, BFMachine p y t c w b))

incCyc :: StateT BFMachine IO ()
incCyc = do
    cyc <- getCyc
    writeCyc (cyc+1)

getBrax :: StateT BFMachine IO (Map Int Int)
getBrax = state (\ (BFMachine p y t c w b) -> (b, BFMachine p y t c w b))

getBracketMatch :: Int -> StateT BFMachine IO Int
getBracketMatch loc = do
    b <- getBrax
    return $ case Data.Map.lookup loc b of 
        Just loc' -> loc'
        Nothing   -> error $ "Error finding bracket match " ++ (show loc)

-- Naive evaluation: just work on the basic BFChars.

evalWithoutAdvance :: BFChar -> StateT BFMachine IO ()
evalWithoutAdvance BPlus = do
    writeLoc <- getWH
    val <- tapeRead writeLoc
    tapeWrite writeLoc ((val+1) `mod` 256)
evalWithoutAdvance BMinus = do
    writeLoc <- getWH
    val <- tapeRead writeLoc
    tapeWrite writeLoc ((val-1) `mod` 256)
evalWithoutAdvance BLeft = do
    wh <- getWH
    writeWH (wh-1)
evalWithoutAdvance BRight = do
    wh <- getWH
    writeWH (wh+1)
evalWithoutAdvance BLBrack = do
    pc <- getPC
    wh <- getWH
    val <- tapeRead wh
    targ <- getBracketMatch pc
    when (val == 0) (writePC targ)
evalWithoutAdvance BRBrack = do
    pc <- getPC
    targ <- getBracketMatch pc
    writePC (targ - 1)    
evalWithoutAdvance BIn = do
    wh <- getWH
    liftIO (putStr "Input (0-255): ")
    inp <- liftIO getLine
    -- error (show inp)
    case (readMaybe inp) of 
        Just n  -> tapeWrite wh n
        Nothing -> error "Invalid input"
evalWithoutAdvance BOut = do
    writeLoc <- getWH
    val <- tapeRead writeLoc
    liftIO (putChar (chr val))

naiveEvalR :: StateT BFMachine IO ()
naiveEvalR = do
    -- for debugging: print state each inst
    -- mach <- Control.Monad.State.get
    -- Control.Monad.State.lift $ putStrLn (show mach)
    -- [end debug stuff]
    cmd <- getCurrCmd
    if (cmd == BEnd)
        then return ()
    else do 
        evalWithoutAdvance cmd
        advancePC
        incCyc
        naiveEvalR

naiveEval :: BFMachine -> IO BFMachine
naiveEval mach = execStateT naiveEvalR mach