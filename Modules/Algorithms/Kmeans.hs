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

kmeans ::[(String,[Float])] -> [Float]-> Int ->IO(String)
kmeans test point n = do 

	let z = calcDists test point []
	let y = sortBy (compare `on` snd) z
	let w = L.take n y
	let k = toList $ fromListWith (+) [((fst c),1)|(c)<-w]
	let sortedList = sortBy (comparing (Down . snd)) k
	print sortedList
	return $ fst (sortedList!!0)

euclidDist :: Floating a => [a] -> [a] -> a
euclidDist v0 v1 = sqrt $ sum [ (x - y) ^ (2::Int) | (x, y) <- zip v0 v1 ]

calcDists :: [(String,[Float])] -> [Float] -> [(String,Float)] -> [(String,Float)]
calcDists [] point category = category
calcDists (x:xs) point category = calcDists xs point ((fst x, euclidDist (snd x) point):category)

