{-# LANGUAGE OverloadedStrings #-}

-- Aim : Getting keywords of various fields of a category and storing the numeric data of TrainSet in database
import System.IO
import Control.Monad
import Data.Char(toUpper)
import Data.List
import Data.Function
import NLP.Stemmer

import qualified ParseBrand as Pb
import qualified ParseFeature as Pf
import qualified ParseTitle as Pt
import qualified NumericData as Nd


--Importing modules to use database
import           Control.Applicative

import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

data TestField = TestField Int Int Int Int Int Int Int Int Int
                 deriving (Show)
--Creating Fields (Columns) 
instance FromRow TestField where
  fromRow = TestField <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow TestField where
  toRow (TestField attribute1_ attribute2_ attribute3_ attribute4_ attribute5_ attribute6_ attribute7_ attribute8_ attribute9) = toRow (attribute1_, attribute2_, attribute3_, attribute4_, attribute5_, attribute6_, attribute7_, attribute8_, attribute9)


--Main Function
main :: IO()
main = do
 		
 		--Getting the Keywords from the files
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

 		--List of lists of keywords of different categories
		let list = [automotiveBrands,electronicsBrands,homeBrands,automotiveFeatures,electronicsFeatures,homeFeatures,automotiveTitle,electronicsTitle,homeTitle];
		
		--Converting the data from TrainSet to numeric data
		home_numericdata<-Nd.ndata file1 list [];
		electronics_numericdata<-Nd.ndata file2 list [];
		automobiles_numericdata<-Nd.ndata file3 list [];


		hClose file1
		hClose file2
		hClose file3
		hClose testFile;

		--Creating database to store the product details in numeric form
		conn1 <- open "Home.db";
  		execute_ conn1 "CREATE TABLE IF NOT EXISTS item (attribute1 INTEGER, attribute2 INTEGER, attribute3 INTEGER, attribute4 INTEGER, attribute5 INTEGER, attribute6 INTEGER, attribute7 INTEGER, attribute8 INTEGER, attribute9 INTEGER)"
  		addToDb conn1 home_numericdata

		conn2 <- open "Electronics.db";
  		execute_ conn2 "CREATE TABLE IF NOT EXISTS item (attribute1 INTEGER, attribute2 INTEGER, attribute3 INTEGER, attribute4 INTEGER, attribute5 INTEGER, attribute6 INTEGER, attribute7 INTEGER, attribute8 INTEGER, attribute9 INTEGER)"
  		addToDb conn2 electronics_numericdata

		conn3 <- open "Automobiles.db";
  		execute_ conn3 "CREATE TABLE IF NOT EXISTS item (attribute1 INTEGER, attribute2 INTEGER, attribute3 INTEGER, attribute4 INTEGER, attribute5 INTEGER, attribute6 INTEGER, attribute7 INTEGER, attribute8 INTEGER, attribute9 INTEGER)"
  		addToDb conn3 automobiles_numericdata

  		close conn1
		close conn2
		close conn3
		



--Function to add entries in a table
addToDb :: Connection -> [[Int]] -> IO()
addToDb conn [] = print "SuccessFull"
addToDb conn (v:vs) = 
	do 
		execute conn "INSERT INTO item (attribute1, attribute2, attribute3, attribute4, attribute5, attribute6, attribute7, attribute8, attribute9) VALUES (?,?,?,?,?,?,?,?,?)" ((v!!0)::Int,(v!!1)::Int,(v!!2)::Int,(v!!3)::Int,(v!!4)::Int,(v!!5)::Int,(v!!6)::Int,(v!!7)::Int,(v!!8)::Int)
	 	addToDb conn vs











