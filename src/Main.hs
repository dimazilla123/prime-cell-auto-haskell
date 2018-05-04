module Main (
    main
) where

import System.Environment
import Rule
import Ring
import Universe

inputCicle :: Int -> Universe -> IO ()
inputCicle times u = do
    if times == 0
       then putStrLn ""
       else do
        print u
        inputCicle (times - 1) . update $ u

prompt :: String -> IO String
prompt str = do
    putStr str
    getLine

main :: IO ()
main = do
    [ruleStr, sizeStr, timesStr] <- getArgs
    let rule = read ruleStr :: Int
    let size = read sizeStr :: Int
    let times = read timesStr :: Int
    let u = Univ (Ring (True:replicate (size - 1) False)) rule
    inputCicle times u
