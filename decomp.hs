import System.Environment as Env
import Data.List.Split as Split

tildeSplit :: [Char] -> [[Char]]
tildeSplit str = Split.splitOn "~" str

parseMap :: String -> [[Char]]
parseMap str = Split.splitOn " " (foldl (++) "" $ init $ tildeSplit str)

parseContent :: String -> [Int]
parseContent str = map (\x -> read x) $ init $ Split.splitOn "," $ last $ tildeSplit str

decomp :: [String] -> [Int] -> String
decomp strs ints = foldl (++) "" $ map (\x -> (strs !! x) ++ " ") ints

getFilePaths :: [String] -> (String, String)
getFilePaths args = if length args >= 2 then ((args !! 0), (args !! 1)) else error "USAGE: decomp [input file] [output file]"

main = do args <- Env.getArgs
          str <- readFile $ fst $ getFilePaths args
          writeFile (snd $ getFilePaths args) $ decomp (parseMap str) (parseContent str)