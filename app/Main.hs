module Main where

import BFEvalBase
import BFRep
import BFTokenEval
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
        -- UNCOMMENT THESE LINES TO DO NAIVE EVAL
        -- let mach = initMachine (toBFChars program)
        -- outState <- naiveEval mach
        -- UNCOMMENT THESE LINES TO DO FANCY EVAL
        let mach = initTokMachine (makeCollapsedTokens program)
        outMach <- eval mach
        putStrLn "\n"
        return ()
