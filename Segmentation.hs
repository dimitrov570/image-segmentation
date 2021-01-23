module Segmentation  where

import Image
import System.IO
import Data.Char
import System.Environment

-- imageSegmentation :: Image -> Image


selectWithComparator :: (a -> a -> a) -> [a] -> a
selectWithComparator cmp lst = foldl1 (\acc x -> cmp acc x) lst

main = do
    args <- getArgs
    progName <- getProgName
    case args of (pathFrom:nrOfClusters:pathTo:[]) -> if (read nrOfClusters :: Int) < 1 then error "Number of clusters must be greater than 0"
                                                                                        else do
                                                                                            img <- loadImage pathFrom
                                                                                            saveImage pathTo img
                 _                                  -> error ("Usage: " ++ progName ++ " inputFilePath numberOfClusters outputFilePath") 
                                    
    