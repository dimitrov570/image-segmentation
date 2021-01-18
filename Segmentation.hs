module Segmentation  where

import Data.Word
import System.IO
import Data.Char

data Rgb = Rgb { red   :: Word8
               , green :: Word8
               , blue  :: Word8 } deriving (Show, Read, Eq)

{-- instance Show Rgb where
    show (Rgb r g b) = show r ++ " " ++ show g ++ " " ++ show b
--}

{- function is used in saveImage -}
printColor :: Rgb -> String
printColor (Rgb r g b) = show r ++ " " ++ show g ++ " " ++ show b

data Pixel = Pixel { color   :: Rgb
                   , cluster :: Rgb
                   , distance  :: Double } deriving (Show,Read)


data Image = Image { width   :: Int
                   , height  :: Int
                   , content :: [[Rgb]] } deriving (Show, Read, Eq)

{-- actually, this function multiplies any Integral number, but returns Word8 --}
multWord8WithFrac :: (RealFrac a) => Word8 -> a -> Word8
multWord8WithFrac w n = round (fromIntegral w * n) :: Word8


{-- Б --}

clamp :: Int -> Word8
clamp n 
    | n > 255 = maxBound :: Word8
    | n < 0 = 0
    | otherwise = fromIntegral n

{- Transforms list of lists of Rgb into list of Pixel -}
rgbToPixel :: [[Rgb]] -> [Pixel]
rgbToPixel [] = []
rgbToPixel xs = map (\x -> Pixel x (Rgb 0 0 0) maxDist) $ concat xs
                    where maxDist = fromIntegral (maxBound :: Int)

-- imageSegmentation :: Image -> Image


{- FUNCTION FOR SAVING IMAGE TO FILE -}

saveImage :: FilePath -> Image -> IO()
saveImage path img = do 
                    writeFile path header
                    appendFile path (maxColorValue ++ "\n")
                    appendFile path $ unlines (map printColor pixels)
                    where header = "P3\n" ++ show (width img) ++ " " ++ show (height img) ++ "\n"
                          maxColorValue = show 255
                          pixels = concat $ content img

{-- Д --}

getWidth :: [String] -> Int
getWidth ln = read (ln !! 0) :: Int

getHeight :: [String] -> Int
getHeight ln = read (ln !! 1) :: Int

extractRow :: (Eq t, Num t) => t -> [String] -> [Rgb]
extractRow 0 _ = []
extractRow _ [] = []
extractRow w lst@(r:g:b:rest) = (Rgb (read r :: Word8) (read g :: Word8) (read b :: Word8)) : extractRow (w - 1) rest 
extractRow _ _ = error "Wrong file structure!"

extractColors :: (Eq t, Num t) => Int -> t -> [String] -> [[Rgb]]
extractColors _ 0 _ = []
extractColors w h lst = extractRow w lst : extractColors w (h - 1) (drop (w * 3) lst)

parseImg :: [String] -> Image
parseImg (f:x:y:xs) 
    | f /= "P3" = error "Unknown format"
    | otherwise = (Image width height pixels)
                        where firstLineWords = (words x)
                              width = getWidth firstLineWords
                              height = getHeight firstLineWords
                              pixels = extractColors width height $ words (unlines xs)


loadImage :: String -> IO Image
loadImage path = do contents <- readFile path
                    let linesOfFile = lines contents
                        img = parseImg linesOfFile
                    return img

comp pathFrom pathTo = do
                        img <- loadImage pathFrom
                        let pixels = rgbToPixel (content img)
                        print pixels