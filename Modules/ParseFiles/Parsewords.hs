-- file: ch07/toupper-imp.hs
module Parsewords 
(
  parse
)
where

import System.IO
import Data.Char(toLower)
import Data.List.Split
import Text.Regex.Posix
import Data.List
import Data.Function
import NLP.Stemmer

parse :: String -> IO([(String,Int)])
parse a =  do 
        inh <- openFile (a++".txt") ReadMode
        outh <- openFile (a++"Feature.txt") WriteMode
        mainloop inh outh 
        hClose outh
        outh <- openFile (a++"Feature.txt") ReadMode
        var <- hGetContents outh;
        let x = splitOneOf ";./:!,-+ $@()[]{}#%^&?<>*  \n" var;
        print (length x)
        let emptylist = []
        let list = regex x emptylist 
        print (length list)
        file <- openFile ("sw.txt") ReadMode
        file2 <- openFile (a++"FeatureKeywords.txt") WriteMode
        sw <- hGetContents file;
        let stopwords =  words sw;
        let result = []
        let noStopwords = removeStopWords list stopwords result;
        print (length noStopwords)
        let emptylist2 = [];
        let finalwords = stemwords noStopwords emptylist2
        print (length finalwords)
        -- let uniquewords = nub finalwords
        -- print (length uniquewords)
        let text = unlines finalwords
        hPutStrLn file2 text
        let wdfreq = g finalwords
        print (length wdfreq)
        let wdtuple = sortBy (compare `on` snd) wdfreq;
        --print wdtuple
        hClose inh
        return $ wdtuple
        

mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
    do ineof <- hIsEOF inh;
       if ineof 
           then return ()
           else do 
                {
                    inpStr <- hGetLine inh;
                if take 8 inpStr == "Feature="
                      then hPutStrLn outh (map toLower (drop 8 inpStr));
                      else hPutStr outh "";
                    mainloop inh outh
                }

regex :: [String] -> [String] -> [String]
regex [] emptylist = emptylist
regex (x:xs) emptylist = if x=~"[0-9]+" || x==""
                            then (regex xs emptylist)
                            else regex xs (x:emptylist)


stemwords :: [String] -> [String] -> [String] 
stemwords [] lst = lst
stemwords (x:xs) lst =  stemwords xs ((stem English x):lst)

removeStopWords :: [String] -> [String] -> [String] ->[String]
removeStopWords [] stopwords result = result
removeStopWords (x:xs) stopwords result = if (x `elem` stopwords)
                      then (removeStopWords xs stopwords result)
                      else (removeStopWords xs stopwords (x:result))

g :: [String] -> [(String,Int)] 
g s = map (\x->(head x, length x)) . group . sort $ s              
