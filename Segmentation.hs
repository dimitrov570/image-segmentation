module Segmentation  where

import Image
import System.IO
import Data.Char
import System.Environment

imageSegmentation :: (Num a) => Image -> a -> Image
imageSegmentation img@(Image w h pixels) nrOfClusters = img

selectWithComparator :: (a -> a -> a) -> [a] -> a
selectWithComparator cmp lst = foldl1 (\acc x -> cmp acc x) lst

getElementsWithStep :: Int -> Int -> [[Pixel]] -> [[Pixel]]
getElementsWithStep _ _ []     = []
getElementsWithStep 0 s (x:xs) = x:(getElementsWithStep s s xs)
getElementsWithStep n s (x:xs) = getElementsWithStep (n - 1) s xs 

extractClusters :: Int -> Int -> Int -> [[Pixel]] -> [Pixel] 
extractClusters _ _ _ [] = []
extractClusters s r limit (x:xs) = (x !! index):(extractClusters s (r+1) limit xs)
                            where index = overloadInt (s * r) limit 

overloadInt :: Int -> Int -> Int
overloadInt n limit = if n > limit then (n - limit)
                                   else n

initClusters :: Int -> [[Pixel]] -> [Pixel]
initClusters c pixels = extractClusters step 0 (length (pixels !! 0)) $ take c (getElementsWithStep 0 step pixels)
                where step    = (length pixels `div` c) - 1


selectclstrs :: Int -> [Int] -> [Int]
selectclstrs c lst@(x:y:xs) = take c (filter (\x -> x `mod` m == 0) lst)
                    where l = length lst
                          m = l `div` c

main = do
    args <- getArgs
    progName <- getProgName
    case args of (pathFrom:clusters:pathTo:[]) -> if nrOfClusters < 1 then error "Number of clusters must be greater than 0"
                                                                                        else do
                                                                                            img <- loadImage pathFrom
                                                                                            print $ initClusters nrOfClusters (content img)
                                                                                            let resultImg = imageSegmentation img nrOfClusters
                                                                                            saveImageClusters pathTo resultImg
                                                                                        where nrOfClusters = read clusters :: Int
                 _                                  -> error ("Usage: " ++ progName ++ " inputFilePath numberOfClusters outputFilePath")