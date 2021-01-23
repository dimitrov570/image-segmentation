module Segmentation  where

import Image
import System.IO
import Data.Char
import System.Environment

imageSegmentation :: (Num a) => Image -> a -> Image
imageSegmentation img@(Image w h pixels) nrOfClusters = img

selectWithComparator :: (a -> a -> a) -> [a] -> a
selectWithComparator cmp lst = foldl1 (\acc x -> cmp acc x) lst



main = do
    args <- getArgs
    progName <- getProgName
    case args of (pathFrom:clusters:pathTo:[]) -> if nrOfClusters < 1 then error "Number of clusters must be greater than 0"
                                                                                        else do
                                                                                            img <- loadImage pathFrom
                                                                                            let resultImg = imageSegmentation img nrOfClusters
                                                                                            saveImageColors pathTo resultImg
                                                                                        where nrOfClusters = read clusters :: Int
                 _                                  -> error ("Usage: " ++ progName ++ " inputFilePath numberOfClusters outputFilePath")
                    
                                                        