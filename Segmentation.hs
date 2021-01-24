module Segmentation  where

import Image
import System.IO
import Data.Char
import System.Environment
import qualified Data.Map as Map  

imageSegmentation :: Image -> Int -> Image
imageSegmentation img@(Image w h pixels) nrOfClusters = (Image w h (setNewClusters pixels $ initClusters nrOfClusters pixels))

recomputeClusters :: [[Pixel]] -> [Rgb] -> ([Rgb], Bool)
recomputeClusters pixels oldClusters = (newClusters, isChanged)
                            where clusterMap                                                 = Map.fromList $ map (\x -> (x, ((LargeRgb 0 0 0),0))) oldClusters
                                  updatedMap                                                 = foldl updateHelper clusterMap $ concat pixels
                                  updateHelper                                               = (\acc x -> Map.insertWith inserter (cluster x) (transformRgb $ color x) acc)
                                  inserter ((LargeRgb r1 g1 b1),n1) ((LargeRgb r2 g2 b2),n2) = ((LargeRgb (r1 + r2) 
                                                                                                          (g1 + g2)
                                                                                                          (b1 + b2)),
                                                                                                n1 + n2)  
                                  transformRgb rgb                                           = ((toLarge rgb),1)   
                                  newClusters                                                = map (\(_, ((LargeRgb r g b), n)) -> (Rgb (clamp $ round (fromInteger r / n))
                                                                                                                                        (clamp $ round (fromInteger g / n))
                                                                                                                                        (clamp $ round (fromInteger b / n)))) $ Map.toList updatedMap
                                  isChanged                                                  = oldClusters /= newClusters

overloadInt :: Int -> Int -> Int
overloadInt n limit = if n > limit then (n - limit)
                                   else n

{- returns list of every "s"-th row starting from 0 -}
getPixelRowsWithStep :: Int -> Int -> [[Pixel]] -> [[Pixel]]
getPixelRowsWithStep _ _ []     = []
getPixelRowsWithStep 0 s (x:xs) = x:(getPixelRowsWithStep s s xs)
getPixelRowsWithStep n s (x:xs) = getPixelRowsWithStep (n - 1) s xs 

{- extracts "s*r"-th pixel from list of pixels, where "s" is step, "r" is current row -}
extractClusters :: Int -> Int -> Int -> [[Pixel]] -> [Rgb] 
extractClusters _ _ _ [] = []
extractClusters s r limit (x:xs) = (color (x !! index)):(extractClusters s (r+1) limit xs)
                            where index = overloadInt (s * r) limit 

{- selects pixels from image as inital clusters at the begining -}
initClusters :: Int -> [[Pixel]] -> [Rgb]
initClusters c pixels = extractClusters step 0 (length (pixels !! 0) - 1) $ take c (getPixelRowsWithStep 0 step pixels)
                where step    = (length pixels `div` c) - 1

rgbDistance :: Rgb -> Rgb -> Integer
rgbDistance (Rgb r1 g1 b1) (Rgb r2 g2 b2) = (toInteger (diff r1 r2))^2 + (toInteger (diff g1 g2))^2 + (toInteger (diff b1 b2))^2
                                where diff a b = if a > b then a - b
                                                          else b - a

{- returns one of the first two colors given as arguments,
    which is closer to the color given as third argument -}
closerColor :: Rgb -> Rgb -> Rgb -> Rgb
closerColor from1 from2 to
                        | d1 < d2   = from1
                        | otherwise = from2
                where d1 = rgbDistance from1 to
                      d2 = rgbDistance from2 to

{- returns closest cluster to a given color as first argument
    from list of clusters given as second argument  -}
selectClosestCluster :: Rgb -> [Rgb] -> Rgb
selectClosestCluster color clusters = foldl1 (\acc x -> closerColor acc x color) clusters

{- sets closest cluster to pixel given as second argument
   from list of clusters given as first argument -}
setClosestCluster :: [Rgb] -> Pixel -> Pixel
setClosestCluster clusters (Pixel color _ _) = Pixel color newCluster distance
                                        where newCluster = selectClosestCluster color clusters
                                              distance = rgbDistance color newCluster

setNewClusters :: [[Pixel]] -> [Rgb] -> [[Pixel]]
setNewClusters pixels clusters = map (map (setClosestCluster clusters)) pixels

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
                 _                             -> error ("Usage: " ++ progName ++ " inputFilePath numberOfClusters outputFilePath")