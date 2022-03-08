module Main where

import BFEval
import BFRep
import Lib
import System.Environment
import System.IO

{-
1. Load the BF file into memory
2. Translate the BF file into an IR
3. Simplify the IR (maybe during step 2?)
4. Output a "compiled" file
-}

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    if (args == []) 
        then error "No input file given"
    else do
        let filepath = head args
        program <- readFile filepath
        let mach = initMachine (toBFChars program)
        outState <- naiveEval mach
        putStrLn "\n"
        -- putStrLn ("\n" ++ show outState)
        return ()
