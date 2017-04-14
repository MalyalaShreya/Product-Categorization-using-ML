module NumericData 
(
  ndata
)
where

import System.IO
import Data.Char(toLower)
import Data.List.Split
import Text.Regex.Posix
import Data.List
-- import qualified Parsewords as Pw
import Data.Function
import NLP.Stemmer

ndata :: Handle -> [[(String,Int)]] -> [[Int]] ->IO([[Int]])
ndata file kw result = do
   ineof <- hIsEOF file;
   if ineof 
           then return $ result
           else do
              
                item <- hGetLine file;
                brand <- hGetLine file;
                feature <- hGetLine file;
                title <- hGetLine file; 
                --print (map toLower (drop 6 brand));
                let brandfreq1 = funcbrand (map toLower (drop 6 brand)) (kw!!0);
                let brandfreq2 = funcbrand (map toLower (drop 6 brand)) (kw!!1);
                let brandfreq3 = funcbrand (map toLower (drop 6 brand)) (kw!!2);

                let featurefreq1 = funcFreq (parseFeature feature) (kw!!3) 0;
                let featurefreq2 = funcFreq (parseFeature feature) (kw!!4) 0;
                let featurefreq3 = funcFreq (parseFeature feature) (kw!!5) 0;


                let titlefreq1 = funcFreq (parseTitle (drop 6 title)) (kw!!6) 0;
                let titlefreq2 = funcFreq (parseTitle (drop 6 title)) (kw!!7) 0;
                let titlefreq3 = funcFreq (parseTitle (drop 6 title)) (kw!!8) 0;

                let list = [brandfreq1, brandfreq2, brandfreq3, featurefreq1, featurefreq2, featurefreq3, titlefreq1, titlefreq2,titlefreq3];
                let r = list:result
                ndata file kw r


              

parseFeature :: String -> [String]
parseFeature str = 
  let
    word = splitOneOf "=. " str;
    var=filter(\y->not(y=="Feature"))word;
    s = unwords var;
    sw = map toLower s; 
    x = splitOneOf ";./:!,-+ $@()[]{}#%^&?<>*  \n" sw;
    finalwords = stemwords x []
    in finalwords

parseTitle :: String -> [String]
parseTitle str = 
  let 
    sw = map toLower str; 
    x = splitOneOf ";./:!,-+ $@()[]{}#%^&?<>*  \n" sw;
    finalwords = stemwords x []
  in finalwords


funcbrand :: String-> [(String,Int)]-> Int
funcbrand str [] = 0
funcbrand str (x:xs) = if str == (fst x)
            then (snd x)
            else funcbrand str xs

funcFreq :: [String] -> [(String,Int)] -> Int ->Int
funcFreq [] keywords val = val
funcFreq (x:xs) keywords val= 
            let 
              var = funcmatch x keywords
              val2 = var + val
            in funcFreq xs keywords val2


funcmatch :: String -> [(String,Int)] ->Int
funcmatch x [] = 0              
funcmatch x (y:ys) = if (x == (fst y)) then (snd y) else (funcmatch x ys)

stemwords :: [String] -> [String] -> [String] 
stemwords [] lst = lst
stemwords (x:xs) lst =  stemwords xs ((stem English x):lst)
