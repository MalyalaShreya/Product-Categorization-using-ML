module NeuralNetworks
(
	nn
) where

import System.IO
import Data.List
import AI.HNN.FF.Network
import Numeric.LinearAlgebra
import qualified Data.Vector.Storable as V

{-nn function implements the Neural Networks classifier on the given tarnaing data and test data.-}
nn :: [[Int]]->[[Int]]->[[Int]]->[[Int]] -> IO([String])
nn automobiles electronics home test = do
	let x = combineHome (listIntToDouble home []) []; -- converting the home training data in the form of [[Int]] to Samples type
	let y = combineElectronics (listIntToDouble electronics []) [];-- converting the electronics training data in the form of [[Int]] to Samples type
	let z = combineAutomobiles (listIntToDouble automobiles [])[];---- converting the automobiles training data in the form of [[Int]] to Samples type
	n <- createNetwork 9 [9] 3    --craeting 3 layer neural network with input layer with 9 neurons  1 hidden layer with 9 neurons and output layer with  neurons
	let w = func x y z [];        --combining all the training data for all the categories
	let n' = trainNTimes 1000 0.7 tanh tanh' n w  -- creating a network which trins for 1000 time with threshold 0.7 using tanh activation function
        let a =result n' (combineTest (listIntToDouble test []) []) [];  --result of the testdata 
        let x=maxindex a []; -- finding the maximum index of each product of the test data
        let y =category x [];-- dividing into category based on the maxindex 
        return $ y   --returns the list of categories for wihch the products of testset belong 

{-combineHome converts the [[Double]] to type Samples of the neural network -}	  
combineHome :: [[Double]] -> Samples Double -> Samples Double
combineHome [] y = y
combineHome (x:xs) y = combineHome xs ((fromList x, fromList [0,0,1]):y)

{-combinelectronics converts the [[Double]] to type Samples of the neural network -}	 
combineElectronics :: [[Double]] -> Samples Double -> Samples Double
combineElectronics [] y = y
combineElectronics (x:xs) y = combineElectronics xs (( fromList x, fromList [0,1,0]):y)

{-combineAutomobiles converts the [[Double]] to type Samples of the neural network -}	 
combineAutomobiles :: [[Double]] -> Samples Double -> Samples Double
combineAutomobiles [] y = y
combineAutomobiles (x:xs) y = combineAutomobiles xs (( fromList x, fromList [1,0,0]):y)

{-combineHome converts the [[Double]] to type Samples of the neural network -}	 
combineTest :: [[Double]] -> Samples Double -> Samples Double
combineTest [] y = y
combineTest (x:xs) y = combineTest xs ((fromList x,fromList[0,0,0]):y)

{-func combines all the training data-}	 
func :: Samples Double -> Samples Double -> Samples Double -> Samples Double -> Samples Double
func [] [] [] w = w
func (x:xs) (y:ys) (z:zs) w =  func xs ys zs (x:y:z:w)

{-listIntToDouble converts [[Int]] to [[Double]] by calling sublistIntToDouble for each element -}
listIntToDouble ::[[Int]]->[[Double]]->[[Double]]
listIntToDouble[] ys = ys 
listIntToDouble (x:xs) ys = let
                        y= sublistIntToDouble x 
                        w=ys++[y] 
                        in listIntToDouble xs w
{-sublistIntToDouble converts [Int] to [Double] by calling intToDouble on each element of the list-}
sublistIntToDouble :: [Int]->[Double]
sublistIntToDouble xs = [intToDouble (x)|x<-xs]

{-intToDouble converts Int to Double-}
intToDouble :: Int -> Double
intToDouble n = fromInteger (toInteger n)

{-result takes the network and Samples of tsetdata and gives the output vector for each product of the testdata-}
result ::Network Double -> Samples Double -> [Vector Double] ->[Vector Double]
result n' [] z =z
result n' (x:xs) z = let
                     a=output n' tanh  (fst x);
                     b=z++[a];
                     in result n' xs b

{-maxindex takes list of vectors and returns a list of maxindex of all the vectors-}
maxindex :: [Vector Double]->[Int]->[Int]
maxindex [] z=z
maxindex (x:xs) z =let
                   c=V.maxIndex x;
                   b=z++[c];
                   in maxindex xs b
{-category takes the list of all the maxindecies and returns the list of corresponding categories-}
category::[Int]->[String]->[String]
category [] list = list
category (x:xs) list  
                    | x==0 = category xs (list++["Automobiles"])
		    | x==1 = category xs (list++["Electronics"])
		    | x==2 = category xs (list++["Home"])
