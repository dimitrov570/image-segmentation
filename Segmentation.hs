module Segmentation  where

import Image
import System.Directory
import System.Exit
import System.IO
import Data.Char
import System.Environment
import Control.Exception
import Text.Read
import qualified Data.Map as Map  


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

{- calculates Euclidean distance between Rgb colors without square root;
   for optimization distance is stored without applying square root to the sum -}
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

{- given a list of lists of Pixel and a list of clusters assigns new clusters to pixels -}
setNewClusters :: [[Pixel]] -> [Rgb] -> [[Pixel]]
setNewClusters pixels clusters = map (map (setClosestCluster clusters)) pixels

{- given a list of lists of Pixel and list of Rgb colors (oldClusters)
   calculates new clusters -}
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
                                  newClusters                                                = map functionToMap $ Map.toList updatedMap
                                  functionToMap (_, ((LargeRgb r g b), n))                   = if n == 0 then (Rgb 0 0 0) 
                                                                                                         else (Rgb (clamp $ round (fromInteger r / n))
                                                                                                                   (clamp $ round (fromInteger g / n))
                                                                                                                   (clamp $ round (fromInteger b / n)))  
                                  isChanged                                                  = oldClusters /= newClusters

{- initializes clusters and calls repeatNTimes if arguments are correct
   then returns segmentated Image;
   if arguments are invalid error is returned -}
imageSegmentation :: Image -> Maybe Int -> Maybe Int -> Image
imageSegmentation img@(Image w h pixels) (Just nrOfClusters) (Just complexity)
                                                                        | nrOfClusters < 1 = error "numberOfClusters must be greater than 0"
                                                                        | otherwise        = (Image w h resultPixels)
                                                                    where clusters  = initClusters nrOfClusters pixels
                                                                          newPixels = setNewClusters pixels clusters
                                                                          resultPixels
                                                                                    | complexity == 1  = newPixels
                                                                                    | otherwise        = repeatNTimes (clusters, True) newPixels complexity

{- repeats n times reasigning clusters to pixels and recomputing new clusters 
   if n is less or equal than zero repeats until no new clusters are created -}
repeatNTimes :: ([Rgb], Bool) -> [[Pixel]] -> Int -> [[Pixel]]
repeatNTimes _ pixels 1 = pixels
repeatNTimes (clusters, isChanged) pixels n
                                        | isChanged == False  = pixels
                                        | n <= 0              = repeatNTimes newClusters newPixels n
                                        | otherwise           = repeatNTimes newClusters newPixels (n-1)
                                    where newClusters = recomputeClusters pixels clusters
                                          newPixels   = setNewClusters pixels (fst newClusters)

{- function that verifies input arguments and call other functions with proper arguments
   or returns errors -}
processImage :: String -> String -> String -> String -> IO Image
processImage pathFrom clustersNr pathTo complexity
                                            | compl        == Nothing = error "complexity argument must be number"
                                            | nrOfClusters == Nothing = error "numberOfClusters must be number"
                                            | otherwise               = do  img <- loadImage pathFrom
                                                                            let resultImg = imageSegmentation img nrOfClusters compl
                                                                            return resultImg               
                                        where nrOfClusters = readMaybe clustersNr :: Maybe Int
                                              compl        = readMaybe complexity :: Maybe Int

usage :: String -> String
usage progName = "Usage: " ++ progName ++ " inputFilePath numberOfClusters outputFilePath complexity\n\nWhere:\n" 
                    ++ inputFilePathMessage ++ numberOfClustersMessage ++ outputFilePathMessage ++ complexityMesage
inputFilePathMessage :: String
inputFilePathMessage = "inputFilePath: Existing file path of picture in p3 ppm format in the file system\n"

numberOfClustersMessage :: String
numberOfClustersMessage = "numberOfClusters: Positive integer, representing number of clusters\n"

outputFilePathMessage :: String
outputFilePathMessage = "outputFilePath: Valid file path in the file system where result should be stored\n"

complexityMesage :: String
complexityMesage = "complexity: Complexity of the algorithm - number of times recalculating clusters should be made, or negative number if recalculating should continue until no more means are created"

main = do
    args <- getArgs
    progName <- getProgName
    case args of (pathFrom:clustersNr:pathTo:times:[]) -> do
                                                             fileExists <- doesFileExist pathFrom
                                                             if fileExists
                                                                  then do 
                                                                    img <- processImage pathFrom clustersNr pathTo times
                                                                    saveImageClusters pathTo img --instead of saving Image pixel colors, it saves pixel's cluster
                                                                    putStrLn "Success"
                                                                    exitSuccess
                                                                  else do putStrLn "Input file does not exist"
                 _                                     -> putStrLn $ usage progName