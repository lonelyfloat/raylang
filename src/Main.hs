module Main where

import Data.List
import Data.Char ( ord, chr )
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Maybe

-- Datatypes

data ParseData = ParseData {currentCommand :: Int, commands :: [StateT RaylangData IO ()]}

data RayData = RayData {
    vals::[Char], location :: Int
}

data RaylangData = RaylangData {rayData :: RayData, parseData :: ParseData}

-- COMMANDS

ptrIncrement :: Monad m => StateT RaylangData m ()
ptrIncrement = do
    d <- get
    put d {rayData = (rayData d){location = location (rayData d) + 1}}

ptrDecrement :: Monad m => StateT RaylangData m ()
ptrDecrement = do
    d <- get
    put d {rayData = (rayData d){location = location (rayData d) - 1}}

valIncrement :: Monad m => StateT RaylangData m ()
valIncrement = do
    d <- get
    put d {rayData = (rayData d){vals = modifyVals (vals (rayData d)) (location (rayData d))}}
    where modifyVals c i = zipWith (\ a b -> if b == i then  chr (ord a + 1) else a) c [0..]

valDecrement :: Monad m => StateT RaylangData m ()
valDecrement = do
    d <- get
    put d {rayData = (rayData d){vals = modifyVals (vals (rayData d)) (location (rayData d))}}
    where modifyVals c i = zipWith (\ a b -> if b == i then  chr (ord a + 1) else a) c [0..]

loopStart :: Monad m => StateT RaylangData m ()
loopStart = do
    d <- get
    let l = if (vals (rayData d) !! location (rayData d)) == chr 0 then getEndLoc else location (rayData d)
    put d {parseData = (parseData d){currentCommand = l}}
    where getEndLoc = 5

loopEnd :: Monad m => StateT RaylangData m ()
loopEnd = do
    d <- get
    let l = if (vals (rayData d) !! location (rayData d)) == chr 0 then getBeginLoc else location (rayData d)
    put d {parseData = (parseData d){currentCommand = l}}
    where getBeginLoc = 5

inputByte :: StateT RaylangData IO ()
inputByte = do
    d <- get
    c <- liftIO getChar
    put d {rayData= (rayData d){vals=modifyVals (vals (rayData d)) (location (rayData d)) c}}
    where modifyVals c i n = zipWith (\ a b -> if b == i then n else a) c [0..]

outputByte :: StateT RaylangData IO ()
outputByte = do
    d <- get
    liftIO $ putChar (vals (rayData d) !! location (rayData d))

none :: Monad m => StateT RaylangData m ()
none = do
    s <- get
    put s

-- Parsing, commands , etc.

raylangCommands :: [String]
raylangCommands = ["ray", "lib", "raylib", "libray", "rayray", "liblib", "libraylib", "raylibray"]

raylangFuncs :: [StateT RaylangData IO ()]
raylangFuncs = [ptrIncrement, ptrDecrement, valIncrement, valDecrement, loopStart, loopEnd, inputByte, outputByte]

parse :: String -> StateT RaylangData IO ()
parse str = maybe none (raylangFuncs !!) (elemIndex str raylangCommands)

-- toActions :: String -> [RayAction]

initialRayData :: String -> RaylangData
initialRayData str = RaylangData {rayData = RayData {vals=repeat (chr 97), location=0},parseData = ParseData {commands = map parse (words str), currentCommand = 0}}

execute :: StateT RaylangData IO ()
execute = do
    d <- get   
    commands (parseData d) !! currentCommand (parseData d)
    dm <- get
    let l = currentCommand (parseData d) + 1
    if l >= (length . commands . parseData) dm then put dm {parseData = (parseData d){currentCommand = l}} else do
        put dm {parseData = (parseData d){currentCommand = l}} 
        execute

interpret :: String -> IO ()
interpret str = evalStateT execute (initialRayData str)

-- main (with test program)

test :: String
test = "raylib raylib raylibray"

main :: IO ()
main = interpret test