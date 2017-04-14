-- file: ch07/toupper-imp.hs
module Parse 
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
        outh <- openFile (a++"BrandKeywords.txt") WriteMode
        mainloop inh outh 
        hClose outh
        outh <- openFile (a++"BrandKeywords.txt") ReadMode
        var <- hGetContents outh;
        let var2 =  (lines var)
        let wdfreq = g var2
        --print (length wdfreq)
        let wdtuple = sortBy (compare `on` snd) wdfreq;
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
                if take 6 inpStr == "Brand="
                      then hPutStrLn outh (map toLower (drop 6 inpStr));
                      else hPutStr outh "";
                    mainloop inh outh
                }

g :: [String] -> [(String,Int)] 
g s = map (\x->(head x, length x)) . group . sort $ s
                

                
