--Module for navie bayes algorithm

module Bayesian
(
 bayes
)
where
import System.IO
import Data.List
import Data.Function

{-mean function calculates the mean of all the 9 attributes of the trainingdata for each category-}
mean::[[Int]]->[Double]
mean []=[]
mean x = let
         y= map sum . transpose $ x
         w=listIntToDouble y
         z= divnum (length x) w;
         in z

{-function for dividing each element of the list with a given number-}
divnum :: Int->[Double]->[Double]
divnum a [] = []
divnum a xs = [x/ intToDouble (a) |x<-xs]

{-intToDouble function conevrts an int to double-}
intToDouble :: Int -> Double
intToDouble n = fromInteger (toInteger n)

{-Square function takes alist as argument and squares each element of the list and returns the list-}
square :: [Double]->[Double]
square xs = [x^2|x<-xs]

{-listIntToDouble converts list of Int to list of Double by calling intToDouble for each element of the list-}
listIntToDouble :: [Int]->[Double]
listIntToDouble xs = [intToDouble (x)|x<-xs]

{-listtoDouble takes list of list of Int and converts it to list of list of doubles by calling listIntToDouble for each element of the argument list and also squares each element of the inner list (i.e element of argument list)-}
listtoDouble::[[Int]]->[[Double]]->[[Double]]
listtoDouble [] ys = ys 
listtoDouble (x:xs) ys = let
                        y=listIntToDouble x
                        z=square y
                        w=ys++[z] 
                        in listtoDouble xs w

{-Finds the expectation of X^2 -}
expX2::[[Double]]->[Double]
expX2 x = let
             y= map sum . transpose $ x
             z= divnum (length x) y;
          in z

{-substract takes two list and returns the difference between each element of the two lists -}
substract :: [Double]->[Double]->[Double]->[Double]
substract [] [] z =z
substract (x:xs) (y:ys) z = let
                            b=x-y;
                            a=z++[b]
                            in substract xs ys a
{-Given Trainingdata and its mean variance function calculates the variance for each attribute of the trainingset by using the formula var(x)=E(X^2)-(mean)^2-}
variance ::[[Int]]->[Double]->[Double]
variance y z = let
               a=listtoDouble y []
               b=expX2 a
               d=square z
               e=substract b d []
               in e

{-pvalue takes the testdata,mean of the category,variance of the category and calulates the probability that of each attribute given testdata is present in the given category by using the formula p(attr/cate)=(1/sqrt(2pivarince)*e^-(x-mean)^2/(2*variance))-}
pvalue :: [Double] -> [Double] -> [Double] -> [Double]->[Double]
pvalue [] [] [] z=z
pvalue (v:vs) (m:ms) (c:cs) z = let 
                                a=(1/(sqrt (2*pi*c)))*(exp(-(((v-m)^2)/(2*c))))
                                b=z++[a]
                                in pvalue vs ms cs b          
 
{-posterior takes a list of testdata,mean of category,varince of category and finds the posterior value of the category for each testdata in a list.posterior(category)=product of all p(attr/category)-}
posterior :: [[Double]]->[Double]->[Double]->[Double]->[Double]
posterior [] m c z=z
posterior (x:xs) m c z=let
                       p=pvalue x m c [];
                       q=mulpvalue p 1;
                       r=z++[q];
                       in posterior xs m c r  
            
{-coverts [[Int]]->[[Double]] by calling listIntToDouble on each element of [[Int]]-}            
testIntToDouble ::[[Int]]->[[Double]]->[[Double]]
testIntToDouble[] ys = ys 
testIntToDouble (x:xs) ys = let
                        y=listIntToDouble x
                        w=ys++[y] 
                        in testIntToDouble xs w

{-mulpvalue takes a list and multiplies all the elements of the list used in calculating the posterior value-}
mulpvalue::[Double]->Double ->Double
mulpvalue [] a=a
mulpvalue (x:xs) a =let
                    b=x*a;
                    in mulpvalue xs b

{-maxfunction finds the maximum of the posterior values in all the categories and returns the category to which it belongs -}
maxfunc :: [Double]->[Double]->[Double]->[String]->[String]
maxfunc [] [] [] z = z
maxfunc (a:as) (e:es) (h:hs) z = let
                                 p=sortBy(compare `on` snd) [("Automobiles",a),("Electronics",e),("Home",h)];
                                 x=(p!!2);
                                 y=z++[(fst x)];
                                 in maxfunc as es hs y 
                                 
                                 
              
{-bayes function takes the trainingdata of all the categories and implements the navie bayes method-}         
bayes :: [[Int]]->[[Int]]->[[Int]]->[[Int]]->[String]
bayes automobiles electronics home tests = let 
                                               	testDouble=testIntToDouble tests []; --converts testdata in the form of [[Int]] to [[Double]]

                                                a_mean=mean automobiles;   --mean of automobiles category
                                                a_variance=variance automobiles a_mean; --varinceof automobiles category
                                                a_posterior=posterior testDouble a_mean a_variance []; --posterior value of automobiles category for the given testdata

                                                e_mean = mean electronics;  --mean of electronics category
                                                e_variance = variance electronics e_mean; --variance of electronics category
                                                e_posterior = posterior testDouble e_mean e_variance []; --posterior value of electronics category for the given testdata

                                                h_mean=mean home;   --mean of home category
                                                h_variance=variance home h_mean; --variance of home category
                                                h_posterior=posterior testDouble h_mean h_variance [];--posterior value of home category for the given testdata

                                                c =maxfunc a_posterior e_posterior h_posterior []; --finds the maximum of posterior value for the testdata
                                                
                                                --return c
                                            in c
                                                     

