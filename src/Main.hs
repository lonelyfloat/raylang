module Main where

import System.Environment ( getArgs )
import System.IO
import Data.List ( elemIndex )
import Data.Char ( ord, chr )
import Data.Maybe ( fromJust )
import Control.Monad.Trans.State ( evalStateT, put, get, StateT )
import Control.Monad.IO.Class ( MonadIO(liftIO) )

-- Datatypes

-- Parsing data of program
data ParseData = ParseData {currentCommand :: Int, totalCommands :: Int, commands :: [StateT RaylangData IO ()], commandStrs :: [String]}

-- List of values and location for program logic
data RayData = RayData {
    vals::[Char], location :: Int
}

-- All the data used in the interpreter
data RaylangData = RaylangData {rayData :: RayData, parseData :: ParseData}

-- COMMANDS

-- Increments the index of the currently accesed cell in the list vals.
ptrIncrement :: Monad m => StateT RaylangData m ()
ptrIncrement = do
    d <- get
    put d {rayData = (rayData d){location = location (rayData d) + 1}}

-- Decrements the index of the currently accesed cell in the list vals.
ptrDecrement :: Monad m => StateT RaylangData m ()
ptrDecrement = do
    d <- get
    put d {rayData = (rayData d){location = location (rayData d) - 1}}

-- Increments the value of the element at the index of the currently accesed cell in the list vals by 1.
valIncrement :: Monad m => StateT RaylangData m ()
valIncrement = do
    d <- get
    put d {rayData = (rayData d){vals = modifyVals (vals (rayData d)) (location (rayData d))}}
    where modifyVals c i = zipWith (\ a b -> if b == i then  chr (ord a + 1) else a) c [0..]

-- Decrements the value of the element at the index of the currently accesed cell in the list vals by 1.
valDecrement :: Monad m => StateT RaylangData m ()
valDecrement = do
    d <- get
    put d {rayData = (rayData d){vals = modifyVals (vals (rayData d)) (location (rayData d))}}
    where modifyVals c i = zipWith (\ a b -> if b == i then  chr (ord a - 1) else a) c [0..]

-- Starts a loop that will terminate when the value at the currently accessed position in the vals list is 0.
loopStart :: Monad m => StateT RaylangData m ()
loopStart = do
    d <- get
    let l = if (vals (rayData d) !! location (rayData d)) == chr 0 then getEndLoc d else currentCommand (parseData d)
    put d {parseData = (parseData d){currentCommand = l}}
    where getEndLoc k = currentCommand (parseData k) + (fromJust . elemIndex "liblib") (drop (currentCommand (parseData k)) (commandStrs (parseData k)))

-- Ends a loop that will terminate when the value at the currently accessed position in the vals list is 0.
loopEnd :: Monad m => StateT RaylangData m ()
loopEnd = do
    d <- get
    let l = if (vals (rayData d) !! location (rayData d)) /= chr 0 then getBeginLoc d else currentCommand (parseData d)
    put d {parseData = (parseData d){currentCommand = l}}
    where getBeginLoc k = currentCommand (parseData k) - (fromJust . elemIndex "rayray") (reverse $ take (currentCommand (parseData k) + 1) (commandStrs (parseData k)))

-- Gets an input char from the user and stores it at the currently accessed position in the vals list. 
inputByte :: StateT RaylangData IO ()
inputByte = do
    d <- get
    c <- liftIO getChar
    put d {rayData= (rayData d){vals=modifyVals (vals (rayData d)) (location (rayData d)) c}}
    where modifyVals c i n = zipWith (\ a b -> if b == i then n else a) c [0..]

-- Outputs a char at the currently accessed position in the vals list. 
outputByte :: StateT RaylangData IO ()
outputByte = do
    d <- get
    liftIO $ putChar (vals (rayData d) !! location (rayData d))

-- Parsing, commands , etc.

-- All commands in raylang.
raylangCommands :: [String]
raylangCommands = ["ray", "lib", "raylib", "libray", "rayray", "liblib", "libraylib", "raylibray"]

-- All of the functions mapping to the commands.
raylangFuncs :: [StateT RaylangData IO ()]
raylangFuncs = [ptrIncrement, ptrDecrement, valIncrement, valDecrement, loopStart, loopEnd, inputByte, outputByte]

-- Turns a command string into a raylang function.
parse :: String -> StateT RaylangData IO ()
parse str =  raylangFuncs !! fromJust (elemIndex str raylangCommands)

-- Returns all of the valid commands in the source code.
getValid :: String -> [String]
getValid str = filter (`elem` raylangCommands) (words str)

-- Initial data created from source code.
initialRayData :: String -> RaylangData
initialRayData src = RaylangData {rayData = RayData {vals=repeat (chr 0), location=0},parseData = ParseData {commands = map parse (getValid src), commandStrs = getValid src, currentCommand = 0, totalCommands = length (getValid src)}}

-- Executes all actons in RaylangData. 
execute :: StateT RaylangData IO ()
execute = do
    d <- get   
    commands (parseData d) !! currentCommand (parseData d)
    dm <- get
    -- liftIO $ print (vals (rayData d) !! location (rayData d))
    let l = currentCommand (parseData dm) + 1
    if l >= totalCommands (parseData dm) then put dm {parseData = (parseData dm){currentCommand = l}} else do
        put dm {parseData = (parseData dm){currentCommand = l}} 
        execute

-- Interprets a string into once I/O action.
interpret :: String -> IO ()
interpret str = evalStateT execute (initialRayData str)

-- Test program (temporary, will add file loading)
test :: String
test =  "raylib raylib raylib raylibray                     sets first val to 3 \
\ ray raylib raylib raylib raylib raylibray                 sets second val  to 4 \
\ rayray lib raylib ray libray liblib                       adds the two values, getting 7 \
\ raylib raylib raylib raylib raylib raylib raylib raylib   makes second val 8 \
\ rayray \
\ lib raylib raylib raylib raylib raylib raylib             adds 6 to first val \
\ ray libray                                                subtracts 1 from second val until it is 0 \
\ liblib lib raylibray                                      end loop" -- Adds two numbers


-- main (temporary)
main :: IO ()
main = do
    args <- getArgs
    let filePath = head args
    f <- openFile filePath ReadMode
    src <- hGetContents f
    interpret src