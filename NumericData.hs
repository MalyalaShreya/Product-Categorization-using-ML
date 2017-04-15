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

{-ndata function takes input file list of keywords tuple for each category and attribute and return the numeric data of the products in input file-}
ndata :: Handle -> [[(String,Int)]] -> [[Int]] ->IO([[Int]])
ndata file kw result = do
   ineof <- hIsEOF file;
   if ineof 
           then return $ result  --if end of file return result
           else do
                -- else get for line 1st line item 2nd line brand 3rd line feature and 4th line title
                item <- hGetLine file;
                brand <- hGetLine file;
                feature <- hGetLine file;
                title <- hGetLine file; 
                let brandfreq1 = funcbrand (map toLower (drop 6 brand)) (kw!!0); --find the frequency of the brand of the product in automobiles category
                let brandfreq2 = funcbrand (map toLower (drop 6 brand)) (kw!!1); --find the frequency of the brand of the product in electronics category
                let brandfreq3 = funcbrand (map toLower (drop 6 brand)) (kw!!2); --find the frequency of the brand of the product in home category

                let featurefreq1 = funcFreq (parseFeature feature) (kw!!3) 0; --find the frequency of the feature of the product in automobiles category
                let featurefreq2 = funcFreq (parseFeature feature) (kw!!4) 0; --find the frequency of the feature of the product in electronics category
                let featurefreq3 = funcFreq (parseFeature feature) (kw!!5) 0; --find the frequency of the feature of the product in home category


                let titlefreq1 = funcFreq (parseTitle (drop 6 title)) (kw!!6) 0;--find the frequency of the title of the product in automobiles category
                let titlefreq2 = funcFreq (parseTitle (drop 6 title)) (kw!!7) 0;--find the frequency of the title of the product in electronics category
                let titlefreq3 = funcFreq (parseTitle (drop 6 title)) (kw!!8) 0;--find the frequency of the title of the product in home category

                let list = [brandfreq1, brandfreq2, brandfreq3, featurefreq1, featurefreq2, featurefreq3, titlefreq1, titlefreq2,titlefreq3];--store all the frequencies in a list and append the list to result list
                let r = list:result 
                ndata file kw r --recursively call the function till end of file


              
{-parseFeature takes the feature and parses all the words and returns list of words-}
parseFeature :: String -> [String]
parseFeature str = 
  let
    word = splitOneOf "=. " str; --split the feature on one of the given attributes
    var=filter(\y->not(y=="Feature"))word;
    s = unwords var;
    sw = map toLower s; 
    x = splitOneOf ";./:!,-+ $@()[]{}#%^&?<>*  \n" sw;--split the feature on one of the given attributes
    finalwords = stemwords x [] --stem all the words
    in finalwords  --return list of final words
    
{-parseFeature takes the title and parses all the words and returns list of words-}
parseTitle :: String -> [String]
parseTitle str = 
  let 
    sw = map toLower str; 
    x = splitOneOf ";./:!,-+ $@()[]{}#%^&?<>*  \n" sw;--split the feature on one of the given attributes
    finalwords = stemwords x []  --stem all the words
  in finalwords --return list of final words

{-funcbrand takes the brand name and a list of tuple of brands and frequency of a category and returns the frequency of the given brand in that category-}
funcbrand :: String-> [(String,Int)]-> Int
funcbrand str [] = 0
funcbrand str (x:xs) = if str == (fst x)
            then (snd x)
            else funcbrand str xs


{-funcFreq takes the brand name and a list of tuple of keywords of feature and frequency of a category and returns the total frequency of the given feature in that category-}
funcFreq :: [String] -> [(String,Int)] -> Int ->Int
funcFreq [] keywords val = val
funcFreq (x:xs) keywords val= 
            let 
              var = funcmatch x keywords
              val2 = var + val
            in funcFreq xs keywords val2

{-funcmatch takes a string and a list of tuple of keywords and its frequency seerches for the given string in the list and returns its frequency-}
funcmatch :: String -> [(String,Int)] ->Int
funcmatch x [] = 0              
funcmatch x (y:ys) = if (x == (fst y)) then (snd y) else (funcmatch x ys)

{-stemwords func takes list of words stems each word and return the list of words after stemming -}
stemwords :: [String] -> [String] -> [String] 
stemwords [] lst = lst
stemwords (x:xs) lst =  stemwords xs ((stem English x):lst)
