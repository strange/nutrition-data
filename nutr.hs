module Main (main) where

import System.Environment.UTF8 (getArgs)
import Data.List 
import Text.Printf (printf)
import Data.Char (toLower)

main = do
    needle <- fmap unwords getArgs
    haystacks <- fmap items $ readFile "resources/nutritiondata.csv"
    output $ matches needle haystacks
    return ()

items :: String -> [[String]]
items x = filter ((== 62) . length) $ map (split) $ lines x

matches :: String -> [[String]] -> [[String]]
matches needle haystacks = filter (search needle) haystacks

search :: String -> [String] -> Bool
search needle haystack = or $ map (isPrefixOf (lower needle)) $ tails name
                         where lower = map (toLower)
                               name = lower $ haystack !! 0

output :: [[String]] -> IO ()
output results = mapM_ (putStrLn . format) results

format :: [String] -> String
format result = printf fstring name carbs protein fat
                where name = result !! 0
                      protein = replaceComma $ result !! 4
                      fat = replaceComma $ result !! 5
                      carbs = replaceComma $ result !! 6
                      fstring = "%s\n \tCHO: %s%%\
                                    \ \tPRO: %s%%\
                                    \ \tFat: %s%%\n"
                      replaceComma xs = map (f) xs
                      f x = if x == ',' then '.' else x

split :: String -> [String]
split = foldr (splitter) [""]
        where splitter c acc
                  | c == d, length h == 0 = acc
                  | c == d = "" : acc
                  | otherwise = (c:h) : t
                  where d = ';'
                        (h:t) = acc
