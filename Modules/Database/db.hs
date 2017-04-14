{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative

import           Database.SQLite.Simple
import           Database.SQLite.Simple.FromRow

data TestField = TestField Int Int Int Int Int Int Int Int Int
                 deriving (Show)

instance FromRow TestField where
  fromRow = TestField <$> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field <*> field

instance ToRow TestField where
  toRow (TestField attribute1_ attribute2_ attribute3_ attribute4_ attribute5_ attribute6_ attribute7_ attribute8_ attribute9) = toRow (attribute1_, attribute2_, attribute3_, attribute4_, attribute5_, attribute6_, attribute7_, attribute8_, attribute9)
 
main :: IO ()
main = do
  let var = [[1,2,3,4,5,6,7,8,9],[12,222,23,24,25,26,27,28,29]]
  conn <- open "Item2.db"
  execute_ conn "CREATE TABLE IF NOT EXISTS item1 (attribute1 INTEGER, attribute2 INTEGER, attribute3 INTEGER, attribute4 INTEGER, attribute5 INTEGER, attribute6 INTEGER, attribute7 INTEGER, attribute8 INTEGER, attribute9 INTEGER)"
  addToDb conn var

  
  r <- query_ conn "SELECT * from item1" :: IO [TestField]
  print (getFromDb r [])
  close conn

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


getFromDb :: [TestField] -> [[Int]] -> [[Int]]
getFromDb [] list = list
getFromDb (x:xs) list =  getFromDb xs ((func x):list)


addToDb :: Connection -> [[Int]] -> IO()
addToDb conn [] = print "SuccessFull"
addToDb conn (v:vs) = 
	do 
		execute conn "INSERT INTO item1 (attribute1, attribute2, attribute3, attribute4, attribute5, attribute6, attribute7, attribute8, attribute9) VALUES (?,?,?,?,?,?,?,?,?)" ((v!!0)::Int,(v!!1)::Int,(v!!2)::Int,(v!!3)::Int,(v!!4)::Int,(v!!5)::Int,(v!!6)::Int,(v!!7)::Int,(v!!8)::Int)
	 	addToDb conn vs












		
