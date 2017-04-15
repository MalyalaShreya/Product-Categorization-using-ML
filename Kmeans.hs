module Kmeans
(
  kmeans
) where


import System.IO
import Data.List
import Data.Function
import Data.Map
import Data.Ord
import qualified Data.List as L


kmeans :: [[Int]] -> [[Int]] -> [[Int]] -> [[Int]] -> Int -> [String] -> [String]
kmeans automobiles electronics home [] n categories = categories
kmeans automobiles electronics home test n categories = let
	train = (func (listtofloat automobiles []) "Automobiles" [])++(func (listtofloat electronics []) "Electronics" [])++(func (listtofloat home []) "Home" [])
	
	z = kmeansList train (listtofloat test []) n []
	in z


kmeansList :: [(String,[Float])] -> [[Float]] -> Int -> [String] -> [String]
kmeansList train [] n categories = categories
kmeansList train (t:ts) n categories = kmeansList train ts n (categories++[(kmeanscalculation train t n)])

kmeanscalculation ::[(String,[Float])] -> [Float]-> Int ->String
kmeanscalculation test point n = 
	let 
	   z = calcDists test point []
	   y = sortBy (compare `on` snd) z
	   w = L.take n y
	   k = toList $ fromListWith (+) [((fst c),1)|(c)<-w]
	   sortedList = sortBy (comparing (Down . snd)) k
	in fst (sortedList!!0)

euclidDist :: Floating a => [a] -> [a] -> a
euclidDist v0 v1 = sqrt $ sum [ (x - y) ^ (2::Int) | (x, y) <- zip v0 v1 ]

calcDists :: [(String,[Float])] -> [Float] -> [(String,Float)] -> [(String,Float)]
calcDists [] point category = category
calcDists (x:xs) point category = calcDists xs point (category++[(fst x, euclidDist (snd x) point)])

func :: [[Float]] -> String -> [(String,[Float])] -> [(String,[Float])]
func [] str result = result
func (x:xs) str result = func xs str (result++[(str,x)])

intToFloat :: Int -> Float
intToFloat n = fromInteger (toInteger n)

listIntToFloat :: [Int]->[Float]
listIntToFloat xs = [intToFloat (x)|x<-xs]

listtofloat::[[Int]]->[[Float]]->[[Float]]
listtofloat [] ys = ys 
listtofloat (x:xs) ys = let
                        y=listIntToFloat x
                        w=ys++[y] 
                        in listtofloat xs w













