module Segmentation  where

import Image
import System.IO
import Data.Char
import System.Environment

imageSegmentation :: (Num a) => Image -> a -> Image
imageSegmentation img@(Image w h pixels) nrOfClusters = img

overloadInt :: Int -> Int -> Int
overloadInt n limit = if n > limit then (n - limit)
                                   else n

{-- returns list of every "s"-th row starting from 0 --}
getPixelRowsWithStep :: Int -> Int -> [[Pixel]] -> [[Pixel]]
getPixelRowsWithStep _ _ []     = []
getPixelRowsWithStep 0 s (x:xs) = x:(getPixelRowsWithStep s s xs)
getPixelRowsWithStep n s (x:xs) = getPixelRowsWithStep (n - 1) s xs 


{-- extracts "s*r"-th pixel from list of pixels, where "s" is step, "r" is current row --}
extractClusters :: Int -> Int -> Int -> [[Pixel]] -> [Rgb] 
extractClusters _ _ _ [] = []
extractClusters s r limit (x:xs) = (color (x !! index)):(extractClusters s (r+1) limit xs)
                            where index = overloadInt (s * r) limit 

{-- selects pixels from image as inital clusters at the begining --}
initClusters :: Int -> [[Pixel]] -> [Rgb]
initClusters c pixels = extractClusters step 0 (length (pixels !! 0) - 1) $ take c (getPixelRowsWithStep 0 step pixels)
                where step    = (length pixels `div` c) - 1


selectWithComparator :: (a -> a -> a) -> [a] -> a
selectWithComparator cmp lst = foldl1 (\acc x -> cmp acc x) lst

rgbDistance :: Rgb -> Rgb -> Integer
rgbDistance (Rgb r1 g1 b1) (Rgb r2 g2 b2) = (toInteger (diff r1 r2))^2 + (toInteger (diff g1 g2))^2 + (toInteger (diff b1 b2))^2
                                where diff a b = if a > b then a - b
                                                          else b - a

lessDistance :: Rgb -> Rgb -> Rgb -> Rgb
lessDistance from1 from2 to
                        | d1 < d2   = from1
                        | otherwise = from2
                where d1 = rgbDistance from1 to
                      d2 = rgbDistance from2 to

selectClosestCluster :: Rgb -> [Rgb] -> Rgb
selectClosestCluster color clusters = foldl1 (\acc x -> lessDistance acc x color) clusters

setClosestCluster :: [Rgb] -> Pixel -> Pixel
setClosestCluster clusters (Pixel color _ _) = (Pixel color newCluster distance)
                                        where newCluster = selectClosestCluster color clusters
                                              distance = rgbDistance color newCluster

recalculateClusters :: [[Pixel]] -> [Rgb] -> [[Pixel]]
recalculateClusters pixels clusters = map (\x -> map (setClosestCluster clusters) x) pixels

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