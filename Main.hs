{-# LANGUAGE OverloadedStrings #-}

-- Aim : Getting keywords of various fields of a category and storing the numeric data of TrainSet in database
import System.IO
import Control.Monad
import Data.Char(toUpper)
import Data.List
import Data.Function
import NLP.Stemmer
import Numeric.LinearAlgebra
import qualified Bayesian as Bayes
import qualified NeuralNetworks as NN
import qualified Kmeans as KNN
import Control.Applicative

import Database.SQLite.Simple
import Database.SQLite.Simple.FromRow

data TestField = TestField Int Int Int Int Int Int Int Int Int
                 deriving (Show)
--Creating Fields (Columns) 
instance FromRow TestField where
  fromRow = TestField <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow TestField where
  toRow (TestField attribute1_ attribute2_ attribute3_ attribute4_ attribute5_ attribute6_ attribute7_ attribute8_ attribute9) = toRow (attribute1_, attribute2_, attribute3_, attribute4_, attribute5_, attribute6_, attribute7_, attribute8_, attribute9)
 
main :: IO ()
main = do

  -- Getting data back from database
  conn1 <- open "Automobiles.db"
  data1 <- query_ conn1 "SELECT * from item" :: IO [TestField]
  let automobiles = getFromDb data1 []

  conn2 <- open "Electronics.db"
  data2 <- query_ conn2 "SELECT * from item" :: IO [TestField]
  let electronics = getFromDb data2 []

  conn3 <- open "Home.db"
  data3 <- query_ conn3 "SELECT * from item" :: IO [TestField]
  let home = getFromDb data3 []

  conn4 <- open "TestSet1.db"
  data4 <- query_ conn4 "SELECT * from item" :: IO [TestField]
  let test1 = getFromDb data4 []

  conn5 <- open "TestSet2.db"
  data5 <- query_ conn5 "SELECT * from item" :: IO [TestField]
  let test2 = getFromDb data5 []
 
  conn6 <- open "TestSet3.db"
  data6 <- query_ conn6 "SELECT * from item" :: IO [TestField]
  let test3 = getFromDb data6 []
  
  conn7 <- open "TestSet4.db"
  data7 <- query_ conn7 "SELECT * from item" :: IO [TestField]
  let test4 = getFromDb data7 []
  
  conn8 <- open "TestSet5.db"
  data8 <- query_ conn8 "SELECT * from item" :: IO [TestField]
  let test5 = getFromDb data8 []


  putStrLn "*********************         PRODUCT CATEGORIZATION        **********************"
  putStrLn ""
  putStrLn "Original Product Details: "
  putStrLn "----------------------------------------------------------------------------------"
  putStrLn "| Test Case 1 : [Automobiles, Automobiles, Electronics, Electronics, Home, Home] |"
  putStrLn "| Test Case 2 : [Automobiles, Automobiles, Electronics, Electronics, Home, Home] |"
  putStrLn "| Test Case 3 : [Automobiles, Automobiles, Electronics, Electronics, Home, Home] |"
  putStrLn "| Test Case 4 : [Automobiles, Automobiles, Electronics, Electronics, Home, Home] |"
  putStrLn "| Test Case 5 : [Automobiles, Automobiles, Electronics, Electronics, Home, Home] |"
  putStrLn "----------------------------------------------------------------------------------"


  putStrLn ""
  putStrLn "-------------------------------------------------------------------------------------"
  putStrLn "                                NEURAL NETWORKS"
  category1NN<- NN.nn automobiles electronics home test1
  print category1NN
  category2NN<- NN.nn automobiles electronics home test2
  print category2NN
  category3NN<- NN.nn automobiles electronics home test3
  print category3NN
  category4NN<- NN.nn automobiles electronics home test4
  print category4NN
  category5NN<- NN.nn automobiles electronics home test5
  print category5NN

  putStrLn ""
  putStrLn "                                NAIVE BAYES"
  let category1Bayes = Bayes.bayes automobiles electronics home test1
  print category1Bayes
  let category2Bayes = Bayes.bayes automobiles electronics home test2
  print category2Bayes
  let category3Bayes = Bayes.bayes automobiles electronics home test3
  print category3Bayes
  let category4Bayes = Bayes.bayes automobiles electronics home test4
  print category4Bayes
  let category5Bayes = Bayes.bayes automobiles electronics home test5
  print category5Bayes



  putStrLn ""
  putStrLn "                                 KMEANS"
  let category1Kmeans = KNN.kmeans automobiles electronics home test1 1 []
  print category1Kmeans
  let category2Kmeans = KNN.kmeans automobiles electronics home test2 1 []
  print category2Kmeans
  let category3Kmeans = KNN.kmeans automobiles electronics home test3 1 []
  print category3Kmeans
  let category4Kmeans = KNN.kmeans automobiles electronics home test4 1 []
  print category4Kmeans
  let category5Kmeans = KNN.kmeans automobiles electronics home test5 1 []
  print category5Kmeans
 
  putStrLn "-------------------------------------------------------------------------------------"
  putStrLn "*************************************************************************************"
  close conn1
  close conn2
  close conn3
  close conn4

attr1 :: TestField -> Int
attr1 (TestField a1 _ _ _ _ _ _ _ _) = a1

attr2 :: TestField -> Int
attr2 (TestField _ a2 _ _ _ _ _ _ _) = a2

attr3 :: TestField -> Int
attr3 (TestField _ _ a3 _ _ _ _ _ _) = a3

attr4 :: TestField -> Int
attr4 (TestField _ _ _ a4 _ _ _ _ _) = a4

attr5 :: TestField -> Int
attr5 (TestField _ _ _ _ a5 _ _ _ _ ) = a5

attr6 :: TestField -> Int
attr6 (TestField _ _ _ _ _ a6 _ _ _ ) = a6

attr7 :: TestField -> Int
attr7 (TestField _ _ _ _ _ _ a7 _ _ ) = a7

attr8 :: TestField -> Int
attr8 (TestField _ _ _ _ _ _ _ a8 _ ) = a8

attr9 :: TestField -> Int
attr9 (TestField _ _ _ _ _ _ _ _ a9 ) = a9


func :: TestField -> [Int]
func r = [attr1(r), attr2(r), attr3(r), attr4(r), attr5(r), attr6(r),attr7(r), attr8(r), attr9(r)] 

-- Function to get data from database
getFromDb :: [TestField] -> [[Int]] -> [[Int]]
getFromDb [] list = list
getFromDb (x:xs) list =  getFromDb xs ((func x):list)













		
