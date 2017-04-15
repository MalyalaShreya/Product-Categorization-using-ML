{-# LANGUAGE OverloadedStrings #-}

import System.IO
import Control.Monad
import Data.Char(toUpper)
import Data.List
import Data.Function
import NLP.Stemmer
import qualified Bayesian as Bayes
import qualified NeuralNetworks as NN
import qualified Kmeans as KNN
import qualified NumericData as Nd
import qualified ParseBrand as Pb
import qualified ParseFeature as Pf
import qualified ParseTitle as Pt

main :: IO ()
main = do
		homeBrands <- Pb.parse "Home";
		electronicsBrands <- Pb.parse "Electronics";
		automotiveBrands <- Pb.parse "Automobiles";

		homeFeatures <- Pf.parse "Home";
		electronicsFeatures <- Pf.parse "Electronics";
		automotiveFeatures <- Pf.parse "Automobiles";

		homeTitle <- Pt.parse "Home";
		electronicsTitle <- Pt.parse "Electronics";
		automotiveTitle <- Pt.parse "Automobiles";


		file1 <- openFile "HomeTrain.txt" ReadMode
 		file2 <- openFile "ElectronicsTrain.txt" ReadMode
 		file3 <- openFile "AutomobilesTrain.txt" ReadMode
 		testFile <- openFile "TestFile.txt" WriteMode
 		--List of lists of keywords of different categories
		let list = [automotiveBrands,electronicsBrands,homeBrands,automotiveFeatures,electronicsFeatures,homeFeatures,automotiveTitle,electronicsTitle,homeTitle];
		print "Successful\n"
		--Converting the data from TrainSet to numeric data
		home_numericdata<-Nd.ndata file1 list [];
		print "P1"
		electronics_numericdata<-Nd.ndata file2 list [];
		print "P2"
		automobiles_numericdata<-Nd.ndata file3 list [];
		print "P3"
                
		print "Brand: "
		brand<-getLine
		print "Feature: "
		feature <- getLine
		print "Title: "
		title <- getLine
		print "k(for Kmeans): "
		k <- getLine
		let n = (read k :: Int)

		hPutStrLn testFile "ITEM 1" 
		hPutStrLn testFile ("Brand="++brand)
		hPutStrLn testFile ("Feature="++feature)
		hPutStrLn testFile ("Title="++title)


		hClose file1
		hClose file2
		hClose file3
		hClose testFile;

		putStrLn "***********************         PRODUCT CATEGORIZATION        ***********************"
		testFile <- openFile "TestFile.txt" ReadMode
		test_numericdata<-Nd.ndata testFile list [];
		categoryNN<- NN.nn automobiles_numericdata electronics_numericdata home_numericdata test_numericdata
		putStrLn "-------------------------------------------------------------------------------------"
		putStrLn "Neural Networks: " 
  		print categoryNN;

  		let categoryBayes = Bayes.bayes automobiles_numericdata electronics_numericdata home_numericdata test_numericdata
		putStrLn "Naive Bayes: "
  		print categoryBayes;

  		let categoryKmeans = KNN.kmeans automobiles_numericdata electronics_numericdata home_numericdata test_numericdata n []
		putStrLn "Kmeans: "
  		print categoryKmeans;
  		putStrLn "-------------------------------------------------------------------------------------"










		
