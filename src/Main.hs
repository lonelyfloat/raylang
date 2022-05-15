module Main where

import Data.List
import Data.Char ( ord, chr )
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Maybe

-- Datatypes

data ParseData = ParseData {currentCommand :: Int, totalCommands :: Int, commands :: [StateT RaylangData IO ()], commandStrs :: [String]}

data RayData = RayData {
    vals::[Char], location :: Int
}

data RaylangData = RaylangData {rayData :: RayData, parseData :: ParseData}

getClosestElemIndex :: Eq a => Int -> a -> [a] -> Int
getClosestElemIndex ix x xs = minimum (map ((\ a -> abs (a - ix)) . snd) (filter (\ ys -> fst ys == x) (zip xs [0 .. ])))

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
    where modifyVals c i = zipWith (\ a b -> if b == i then  chr (ord a - 1) else a) c [0..]

loopStart :: Monad m => StateT RaylangData m ()
loopStart = do
    d <- get
    let l = if (vals (rayData d) !! location (rayData d)) == chr 0 then getEndLoc d else currentCommand (parseData d)
    put d {parseData = (parseData d){currentCommand = l}}
    where getEndLoc k = fromJust (elemIndex "liblib" (commandStrs (parseData k)))

loopEnd :: Monad m => StateT RaylangData m ()
loopEnd = do
    d <- get
    let l = if (vals (rayData d) !! location (rayData d)) /= chr 0 then getBeginLoc d else currentCommand (parseData d)
    put d {parseData = (parseData d){currentCommand = l}}
    where getBeginLoc k = fromJust (elemIndex "rayray" (commandStrs (parseData k)))

inputByte :: StateT RaylangData IO ()
inputByte = do
    d <- get
    c <- liftIO getChar
    put d {rayData= (rayData d){vals=modifyVals (vals (rayData d)) (location (rayData d)) c}}
    where modifyVals c i n = zipWith (\ a b -> if b == i then n else a) c [0..]

outputByte :: StateT RaylangData IO ()
outputByte = do
    d <- get
    liftIO $ putChar (chr (ord (vals (rayData d) !! location (rayData d)) + 48))

-- Parsing, commands , etc.

raylangCommands :: [String]
raylangCommands = ["ray", "lib", "raylib", "libray", "rayray", "liblib", "libraylib", "raylibray"]

raylangFuncs :: [StateT RaylangData IO ()]
raylangFuncs = [ptrIncrement, ptrDecrement, valIncrement, valDecrement, loopStart, loopEnd, inputByte, outputByte]

parse :: String -> StateT RaylangData IO ()
parse str =  raylangFuncs !! fromJust (elemIndex str raylangCommands)

getValid :: String -> [String]
getValid str = filter (`elem` raylangCommands) (words str)

-- toActions :: String -> [RayAction]

initialRayData :: String -> RaylangData
initialRayData src = RaylangData {rayData = RayData {vals=repeat (chr 0), location=0},parseData = ParseData {commands = map parse (getValid src), commandStrs = getValid src, currentCommand = 0, totalCommands = length (getValid src)}}

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

interpret :: String -> IO ()
interpret str = evalStateT execute (initialRayData str)

-- main (with test program)

test :: String
test = "raylib raylib raylib raylibray ray raylib raylib raylib raylib raylibray rayray lib raylib ray libray liblib lib raylibray"

main :: IO ()
main = interpret test