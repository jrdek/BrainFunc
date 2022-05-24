module Main where

import BFEvalBase
import BFRep
import BFTokenEval
import System.Environment
import System.IO

{-
1. Load the BF file into memory
2. Translate the BF file into an IR
3. Execute the generated IR code

Since we're interpreting and not compiling, it seems like
we wouldn't get much speedup by performing optimizations.

One really cheeky thing we could do here is transpile to C 
from the IR...
-}


{-
Level 0: 
    completely unoptimized IR. +++ becomes [inc, inc, inc]
Level 1: 
    - compacted instructions (e.g., +++ becomes [Add 3])
    - small NOPs are eliminated (Add 0, Mov 0, While{[]}) 
-}
optimizeLevel = 1

main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    args <- getArgs
    if (args == []) 
        then error "No input file given"
    else do
        let filepath = head args
        program <- readFile filepath
        case optimizeLevel of
            0 -> do
                putStrLn "Using unoptimized compilation.\n"
                let mach = initMachine (toBFChars program)
                outState <- naiveEval mach
                return ()
            1 -> do
                putStrLn "Using compacted compilation.\n"
                let mach = initTokMachine (makeCollapsedTokens program)
                outMach <- eval mach
                return ()
        putStrLn "\n"
        return ()
