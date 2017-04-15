{-# LANGUAGE OverloadedStrings #-}

--Aim : Getting Test data and storing it in a database
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
import           Control.Applicative
import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow


-- Creating fields for the database
data TestField = TestField Int Int Int Int Int Int Int Int Int
                 deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow TestField where
  toRow (TestField attribute1_ attribute2_ attribute3_ attribute4_ attribute5_ attribute6_ attribute7_ attribute8_ attribute9) = toRow (attribute1_, attribute2_, attribute3_, attribute4_, attribute5_, attribute6_, attribute7_, attribute8_, attribute9)

-- Main Function
main :: IO()
main = do
 		
 		-- Preparing List of keywords for different categories
		homeBrands <- Pb.parse "Home";
		electronicsBrands <- Pb.parse "Electronics";
		automotiveBrands <- Pb.parse "Automobiles";

		homeFeatures <- Pf.parse "Home";
		electronicsFeatures <- Pf.parse "Electronics";
		automotiveFeatures <- Pf.parse "Automobiles";

		homeTitle <- Pt.parse "Home";
		electronicsTitle <- Pt.parse "Electronics";
		automotiveTitle <- Pt.parse "Automobiles";

		--List of lists of keywords of different categories
		let list = [automotiveBrands,electronicsBrands,homeBrands,automotiveFeatures,electronicsFeatures,homeFeatures,automotiveTitle,electronicsTitle,homeTitle];


		--Converting the Testdata to numeric data
		testFile1 <- openFile "TestFile1.txt" ReadMode
		test_numericdata1<-Nd.ndata testFile1 list [];

		testFile2 <- openFile "TestFile2.txt" ReadMode
		test_numericdata2<-Nd.ndata testFile2 list [];

		testFile3 <- openFile "TestFile3.txt" ReadMode
		test_numericdata3<-Nd.ndata testFile3 list [];

		testFile4 <- openFile "TestFile4.txt" ReadMode
		test_numericdata4<-Nd.ndata testFile4 list [];

		testFile5 <- openFile "TestFile5.txt" ReadMode
		test_numericdata5<-Nd.ndata testFile5 list [];


		hClose testFile1
		hClose testFile2
		hClose testFile3
		hClose testFile4
		hClose testFile5

		conn1 <- open "TestSet1.db";
  		execute_ conn1 "CREATE TABLE IF NOT EXISTS item (attribute1 INTEGER, attribute2 INTEGER, attribute3 INTEGER, attribute4 INTEGER, attribute5 INTEGER, attribute6 INTEGER, attribute7 INTEGER, attribute8 INTEGER, attribute9 INTEGER)"
  		addToDb conn1 test_numericdata1

		conn2 <- open "TestSet2.db";
  		execute_ conn2 "CREATE TABLE IF NOT EXISTS item (attribute1 INTEGER, attribute2 INTEGER, attribute3 INTEGER, attribute4 INTEGER, attribute5 INTEGER, attribute6 INTEGER, attribute7 INTEGER, attribute8 INTEGER, attribute9 INTEGER)"
  		addToDb conn2 test_numericdata2

		conn3 <- open "TestSet3.db";
  		execute_ conn3 "CREATE TABLE IF NOT EXISTS item (attribute1 INTEGER, attribute2 INTEGER, attribute3 INTEGER, attribute4 INTEGER, attribute5 INTEGER, attribute6 INTEGER, attribute7 INTEGER, attribute8 INTEGER, attribute9 INTEGER)"
  		addToDb conn3 test_numericdata3

		conn4 <- open "TestSet4.db";
  		execute_ conn4 "CREATE TABLE IF NOT EXISTS item (attribute1 INTEGER, attribute2 INTEGER, attribute3 INTEGER, attribute4 INTEGER, attribute5 INTEGER, attribute6 INTEGER, attribute7 INTEGER, attribute8 INTEGER, attribute9 INTEGER)"
  		addToDb conn4 test_numericdata4

		conn5 <- open "TestSet5.db";
  		execute_ conn5 "CREATE TABLE IF NOT EXISTS item (attribute1 INTEGER, attribute2 INTEGER, attribute3 INTEGER, attribute4 INTEGER, attribute5 INTEGER, attribute6 INTEGER, attribute7 INTEGER, attribute8 INTEGER, attribute9 INTEGER)"
  		addToDb conn5 test_numericdata5


  		close conn1
		close conn2
		close conn3
		close conn4
		close conn5
		




addToDb :: Connection -> [[Int]] -> IO()
addToDb conn [] = print "SuccessFull"
addToDb conn (v:vs) = 
	do 
		execute conn "INSERT INTO item (attribute1, attribute2, attribute3, attribute4, attribute5, attribute6, attribute7, attribute8, attribute9) VALUES (?,?,?,?,?,?,?,?,?)" ((v!!0)::Int,(v!!1)::Int,(v!!2)::Int,(v!!3)::Int,(v!!4)::Int,(v!!5)::Int,(v!!6)::Int,(v!!7)::Int,(v!!8)::Int)
	 	addToDb conn vs











