{-# LANGUAGE BangPatterns, StrictData #-}
module BFTokenEval where
    -- TODO: set what functions this exports

import BFRep
import Control.Monad.State
import Data.Char
import Data.Map.Strict as M
import Text.Read

data BFTokMachine = BFTokMachine {
    -- TODO: It seems like a list should be fine for the program?
    prog :: [BFToken],
    -- TODO: do we want "cycles"?
    tape :: Map Int Int,
    -- no need for a PC: the PC will never go backward!
    wh :: Int
    -- no need to know about brackets!
}

instance Show BFTokMachine where
    show (BFTokMachine p t w) =
        ("Tape: " ++ show t) ++ "\n" ++
        ("WH: " ++ show w) ++ "\n"

initTokMachine :: [BFToken] -> BFTokMachine
initTokMachine pl = BFTokMachine pl empty 0

readCurCell :: BFTokMachine -> Int
readCurCell mach = M.findWithDefault 0 (wh mach) (tape mach)

-- NOTE: this advances the "PC"
writeCurCell :: BFTokMachine -> Int -> BFTokMachine
writeCurCell mach n = BFTokMachine (tail $ prog mach) (insert (wh mach) (n `mod` 256) (tape mach)) (wh mach)

-- TODO: what are the consequences of not using state here?
evalTok :: BFTokMachine -> IO BFTokMachine
evalTok !mach = do
    let t = tape mach
    let w = wh mach
    case (prog mach) of
        (BtokAdd n):rest -> do
            let curVal = readCurCell mach
            return $ writeCurCell mach (curVal+n)
        (BtokMov n):rest -> 
            return $ BFTokMachine rest t (w+n)
        (BtokIn):rest -> do
            putStr "Input (0-255): "
            inp <- getLine
            case readMaybe inp of
                Just n -> return $ writeCurCell mach n
                Nothing -> error "Invalid input"
        (BtokOut):rest -> do
            putChar (chr (readCurCell mach))
            return $ BFTokMachine rest t w  -- TODO OH NO
        (BtokWhile block):rest -> do
            case readCurCell mach of
                0 -> return $ BFTokMachine rest t w
                _ -> do
                    machAfterIter <- eval $ BFTokMachine block t w
                    let newTape = tape machAfterIter
                    let newWh = wh machAfterIter
                    return $ BFTokMachine ((BtokWhile block):rest) newTape newWh                    

eval :: BFTokMachine -> IO BFTokMachine
eval !mach = do
    case (prog mach) of
        [] -> return mach
        p -> do
            afterMach <- evalTok mach
            eval afterMach