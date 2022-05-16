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
getClosestElemIndex ix x xs = (fromJust . elemIndex x) (drop ix xs)

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

loopStart :: StateT RaylangData IO ()
loopStart = do
    d <- get
    let l = if (vals (rayData d) !! location (rayData d)) == chr 0 then getEndLoc d else currentCommand (parseData d)
    -- liftIO $ print l
    put d {parseData = (parseData d){currentCommand = l}}
    where getEndLoc k = currentCommand (parseData k) + getClosestElemIndex (currentCommand (parseData k)) "liblib" (commandStrs (parseData k))

loopEnd :: StateT RaylangData IO ()
loopEnd = do
    d <- get
    let l = if (vals (rayData d) !! location (rayData d)) /= chr 0 then getBeginLoc d else currentCommand (parseData d)
    --liftIO $ print l
    put d {parseData = (parseData d){currentCommand = l}}
    where getBeginLoc k = currentCommand (parseData k) - (fromJust . elemIndex "rayray") (reverse $ take (currentCommand (parseData k) + 1) (commandStrs (parseData k)))

inputByte :: StateT RaylangData IO ()
inputByte = do
    d <- get
    c <- liftIO getChar
    put d {rayData= (rayData d){vals=modifyVals (vals (rayData d)) (location (rayData d)) c}}
    where modifyVals c i n = zipWith (\ a b -> if b == i then n else a) c [0..]

outputByte :: StateT RaylangData IO ()
outputByte = do
    d <- get
    --liftIO $ putChar (chr (ord (vals (rayData d) !! location (rayData d)) + 48))
    liftIO $ putChar (vals (rayData d) !! location (rayData d))

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

-- main (with test program)getClosestElemIndex (10) "liblib" (test)

test :: String
test = "raylib raylib raylib raylib raylibray \
\ ray raylib raylib raylib raylib raylibray \
\ rayray lib raylib ray libray liblib \
\ raylib raylib raylib raylib raylib raylib raylib raylib \
\ rayray \
\ lib raylib raylib raylib raylib raylib raylib \
\ ray libray \
\ liblib lib raylibray"

main :: IO ()
main = interpret test
    -- print $ 34 - (fromJust . elemIndex "rayray") (reverse $ take (34 + 1) (getValid test))



{-

 "raylib raylib raylib raylibray                      sets first val to 3 \
\ ray raylib raylib raylib raylib raylibray                 sets second val  to 4 \
\ rayray lib raylib ray libray liblib                       adds the two, getting 7 \
\ raylib raylib raylib raylib raylib raylib raylib raylib   makes second val 8 \
\ rayray \
\ lib raylib raylib raylib raylib raylib raylib             adds 6 to first val \
\ ray libray                                                subtracts 1 from second val \
\ liblib lib raylibray                                      end loop" -- Adds two numbers
-}