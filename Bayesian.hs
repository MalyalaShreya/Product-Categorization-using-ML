module Bayesian 
(
  mean,
  variance,
  pvalue,
  listtofloat,
  expX2,
  substract,
  square,
  bayes,
  posterior
)
where

import System.IO
import Data.List
import Data.Function
mean::[[Int]]->[Float]
mean []=[]
mean x = let
         y= map sum . transpose $ x
         w=listIntToFloat y
         z= divnum 1000 w;
         in z

divnum :: Int->[Float]->[Float]
divnum a [] = []
divnum a xs = [x/ intToFloat (a) |x<-xs]

intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)

square :: [Float]->[Float]
square xs = [x^2|x<-xs]


listIntToFloat :: [Int]->[Float]
listIntToFloat xs = [intToFloat (x)|x<-xs]

listtofloat::[[Int]]->[[Float]]->[[Float]]
listtofloat [] ys = ys 
listtofloat (x:xs) ys = let
                        y=listIntToFloat x
                        z=square y
                        w=ys++[z] 
                        in listtofloat xs w
expX2::[[Float]]->[Float]
expX2 x = let
             y= map sum . transpose $ x
             z= divnum 1000 y;
          in z

substract :: [Float]->[Float]->[Float]->[Float]
substract [] [] z =z
substract (x:xs) (y:ys) z = let
                            b=x-y;
                            a=z++[b]
                            in substract xs ys a

variance ::[[Int]]->[Float]->[Float]
variance y z = let
               a=listtofloat y []
               b=expX2 a
               d=square z
               e=substract b d []
               in e

pvalue :: [Float] -> [Float] -> [Float] -> [Float]->[Float]
pvalue [] [] [] z=z
pvalue (v:vs) (m:ms) (c:cs) z = let 
                                a=(1/(sqrt (2*pi*c)))*(exp(-(((v-m)^2)/(2*c))))
                                b=z++[a]
                                in pvalue vs ms cs b          
 

posterior :: [[Float]]->[Float]->[Float]->[Float]->[Float]
posterior [] m c z=z
posterior (x:xs) m c z=let
                       p=pvalue x m c [];
                       q=mulpvalue p 1;
                       r=z++[q];
                       in posterior xs m c r              
             
testIntToFloat ::[[Int]]->[[Float]]->[[Float]]
testIntToFloat[] ys = ys 
testIntToFloat (x:xs) ys = let
                        y=listIntToFloat x
                        w=ys++[y] 
                        in listtofloat xs w

mulpvalue::[Float]->Float ->Float
mulpvalue [] a=a
mulpvalue (x:xs) a =let
                    b=x*a;
                    in mulpvalue xs b


maxfunc :: [Float]->[Float]->[Float]->[String]->[String]
maxfunc [] [] [] z = z
maxfunc [] [] hs z =z
maxfunc [] es hs z =z
maxfunc (a:as) (e:es) (h:hs) z = let
                                 p=sortBy(compare `on` snd) [("Automobiles",a),("Electronics",e),("Home",h)];
                                 x=(p!!2);
                                 y=z++[(fst x)];
                                 in maxfunc as es hs y
                                 
                                 
                                  
               
          
bayes :: [[Int]]->[[Int]]->[[Int]]->[[Int]]->[String]
bayes automobiles electronics home tests = let 
                                                     testfloat=testIntToFloat tests [];
                                                     automean=mean automobiles;
                                                     autovariance=variance automobiles automean;
                                                     posteriorauto=posterior testfloat automean autovariance [];
                                                     elecmean=mean electronics;
                                                     elecvariance=variance electronics elecmean;
                                                     posteriorelec=posterior testfloat elecmean elecvariance [];
                                                     homemean=mean home;
                                                     homevariance=variance home homemean;
                                                     posteriorhome=posterior testfloat homemean homevariance [];
                                                     string =maxfunc posteriorauto posteriorelec homevariance [];
                                                     in string
                                                     
