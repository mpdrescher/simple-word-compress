import System.Environment as Env
import Data.List.Split as Split
import Data.List as List
import Data.Maybe as Maybe

spaceSplit :: String -> [String]
spaceSplit str = Split.splitOn " " str

getWords :: String -> [String]
getWords str = map (\x -> head x) $ group $ List.sort $ spaceSplit str

comp :: String -> [Int]
comp str = map (\x -> Maybe.fromJust $ List.findIndex (\y -> y == x) $ getWords str) $ spaceSplit str

showWords :: [[Char]] -> [Char]
showWords str = foldl (\acc x -> acc ++ x ++ " ") "" str

showComp :: [Int] -> [Char]
showComp ints = foldl (\acc x -> acc ++ (show x) ++ ",") "" ints

getFilePaths :: [String] -> (String, String)
getFilePaths args = if length args >= 2 then ((args !! 0), (args !! 1)) else error "USAGE: comp [input file] [output file]"

composeOutput :: String -> String
composeOutput str = "" ++ (showWords $ getWords str) ++ "~" ++ (showComp $ comp str)

main = do args <- Env.getArgs
          str <- readFile $ fst $ getFilePaths args
          writeFile (snd $ getFilePaths args) $ composeOutput str