
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

data Word = Word {english :: String, latin :: String} deriving Show

main          :: IO ()
loadList      :: String -> IO [Word]
getWords      :: String -> [Word]
parse         :: String -> Word

main = nouns >>= output
       where nouns = loadList "nouns.txt"
             verbs = loadList "verbs.txt"
             output = mapM_ print 

loadList file = fmap getWords $ readFile file

getWords = (map parse) . lines

parse line = Word (ws !! 0) (ws !! 1)
       where ws = ((map (T.unpack . T.strip)) . (T.splitOn ",") . T.pack) line
       