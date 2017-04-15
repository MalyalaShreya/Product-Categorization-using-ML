-- file: ch07/toupper-imp.hs
module ParseBrand 
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
{-parse function takes the file to be opened as argument and returns list of tuple of string anf Int-}
parse :: String -> IO([(String,Int)])
parse a =  do 
        inh <- openFile (a++".txt") ReadMode  --open the given file in read mode
        outh <- openFile (a++"BrandKeywords.txt") WriteMode --opening a new file Brandkeywords in Write mode
        mainloop inh outh --calling the mainloop function with handles inh and outh
        hClose outh  --close BrandKeywords.txt and open in read mode
        outh <- openFile (a++"BrandKeywords.txt") ReadMode 
        var <- hGetContents outh; --get contents of the file
        let var2 =  (lines var) --on lines function it breaks at \n and gives a list of lines
        let wdfreq = g var2     -- find the frequency of each of the brands in the entire list of brands 
        let wdtuple = sortBy (compare `on` snd) wdfreq; --sort the brands list on the basis of frequency of barnd
        hClose inh  --close input file
        return $ wdtuple -- return list of tuple of brand and its frequency
        
{-mainloop function takes two handles and traverses through the  first file and put contents into the second file -}
mainloop :: Handle -> Handle -> IO ()
mainloop inh outh = 
    do ineof <- hIsEOF inh;  
       if ineof 
           then return () --if end of inh return
           else do 
                {  --else get line 
                    inpStr <- hGetLine inh;
                   --if  the first 6 characters of the getline = "Brand=" then drop these 6 characters and convert to lower and write into outfile
                if take 6 inpStr == "Brand="
                      then hPutStrLn outh (map toLower (drop 6 inpStr));
                      else hPutStr outh ""; -- else write nothing
                    mainloop inh outh   --recursively execute till end of file
                }
{-g finds the frequency of each element in the list and returns a list of tuple with element and its frequency-}
g :: [String] -> [(String,Int)] 
g s = map (\x->(head x, length x)) . group . sort $ s
                

                
