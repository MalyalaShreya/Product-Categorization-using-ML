-- file: ch07/toupper-imp.hs
module ParseTitle
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
{-parse function takes the name of the file to be opened as input and returns a tuple of keywords and its frequency-}
parse :: String -> IO([(String,Int)])
parse a =  do 
        inh <- openFile (a++".txt") ReadMode --open input file in read mode
        outh <- openFile (a++"Title.txt") WriteMode --open a new file in write mode
        mainloop inh outh --calling function mainloop on handles inh and outh
        hClose outh    --close the Title.txt file and open in read mode
        outh <- openFile (a++"Title.txt") ReadMode
        var <- hGetContents outh;  --get contents of the file
        let x = splitOneOf ";./:!,-+ $@()[]{}#%^&?<>*  \n" var;--splitting the contents on one of the givenattribute
        --print (length x)
        let emptylist = []
        let list = regex x emptylist --apply regex to remove unecessary words from the list 
        --print (length list)
        file <- openFile ("sw.txt") ReadMode  --open stopwords file in read mode
        file2 <- openFile (a++"TitleKeywords.txt") WriteMode --open a new file in write mode
        sw <- hGetContents file; --getcontents of stop words file
        let stopwords =  words sw;--apply words function on stopwords contents which gives us list of stop words
        let result = []
        let noStopwords = removeStopWords list stopwords result;--remove stopwords from keywords list
        --print (length noStopwords)
        let emptylist2 = [];
        let finalwords = stemwords noStopwords emptylist2 --appyly stemming on the keywords list
        --print (length finalwords)
        let text = unlines finalwords 
        hPutStrLn file2 text  --storing all the keywords into a file each in a line by unline the keywords list
        let wdfreq = g finalwords --getting the list of tuple of keyword and its frequecy
        --print (length wdfreq)
        let wdtuple = sortBy (compare `on` snd) wdfreq; --sort the list of tuples on the basis of frequency
        hClose inh
        return $ wdtuple  --return the list of tuple of key words and its frequency
        
{-mainloop function takes two handles reads form the first handle and writes into the second handle -}
mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
    do ineof <- hIsEOF inh;
       if ineof 
           then return () --if end of file return
           else do 
                {   --getline
                    inpStr <- hGetLine inh;
                    --if the first 8 characters of inpstr =="Feature=" then drop these 8 letters and lower the remaining string and write  into output file 
                    if take 6 inpStr == "Title="
                      then hPutStrLn outh (map toLower (drop 8 inpStr));
                      else hPutStr outh "";--else write nothing
                    mainloop inh outh --recursively call mainloop till end of file
                }
{-regex function takes list of string and returns list of strings removing the string which are accepted by the given regex -}
regex :: [String] -> [String] -> [String]
regex [] emptylist = emptylist
regex (x:xs) emptylist = if x=~"[0-9]+" || x==""
                            then (regex xs emptylist)
                            else regex xs (x:emptylist)

{-stemwords function takes the list of string and stems each string and returns the final list-}
stemwords :: [String] -> [String] -> [String] 
stemwords [] lst = lst
stemwords (x:xs) lst =  stemwords xs ((stem English x):lst)

{-removeStopWords takes list of strings and stopwords list and returns a list of strings removing all the stopwords from te first argumnet-}
removeStopWords :: [String] -> [String] -> [String] ->[String]
removeStopWords [] stopwords result = result
removeStopWords (x:xs) stopwords result = if (x `elem` stopwords)
                      then (removeStopWords xs stopwords result)
                      else (removeStopWords xs stopwords (x:result))

{-g takes the list of string and returns list of tuples with string and its corresponding frequency in the list-}
g :: [String] -> [(String,Int)] 
g s = map (\x->(head x, length x)) . group . sort $ s              
