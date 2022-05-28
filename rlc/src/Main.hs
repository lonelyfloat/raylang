module Main where

import Control.Monad (when)
import System.Directory ( doesFileExist, removeFile )
import System.Process ( callCommand )
import System.Environment ( getArgs )
import System.IO ( openFile, hGetContents, IOMode(ReadMode) )
import Data.List ( elemIndex )
import Data.Maybe ( fromJust, fromMaybe )

-- Returns all of the valid commands in the source code.
getValid :: String -> [String]
getValid str = filter (`elem` raylangCommands) (words str)

-- All commands in raylang.
raylangCommands :: [String]
raylangCommands = ["ray", "lib", "raylib", "libray", "rayray", "liblib", "libraylib", "raylibray"]

-- C commands corresponding to the raylang commands.
cCommands :: [String]
cCommands = ["++loc;", "--loc;", "++data[loc];", "--data[loc];", "while ((int)data[loc] != 0 || loc < 0 || loc > 3000) {", "}", "data[loc] = getchar();", "putchar(data[loc]);"]

-- Turn a string (raylang code) into C code.
parse :: String -> String
parse src = "#include <stdio.h>\n int main (void){unsigned int loc = 0; unsigned char data[3000]; for(int i = 0; i < 3000; ++i){data[i]=0;} " ++ concatMap (\s -> cCommands !! (fromJust . elemIndex s) raylangCommands) (getValid src) ++ "return 0; }"

getFileName :: String -> String
getFileName str = take (periodLoc (drop (slashLoc str) str) - 1) (drop (slashLoc str + 1) str)
    where periodLoc s = fromMaybe (length s - 1) (elemIndex '.' s)
          slashLoc s = fromMaybe 0 (elemIndex '/' s)
-- "dir/example.asd"
-- main

main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
    f <- openFile filePath ReadMode
    src <- hGetContents f
    -- print $ matchToStart 34 "rayray" "liblib" (getValid src)
    let c = parse src
    let cfile = getFileName filePath
    writeFile (cfile ++ ".c") c
    callCommand ("gcc " ++ (cfile ++ ".c") ++ " -o " ++ cfile)
    fileExist <- doesFileExist (cfile ++ ".c")
    when fileExist (removeFile $ cfile ++ ".c")