module Image where
    
import Data.Word

data Rgb = Rgb { red   :: Word8
               , green :: Word8
               , blue  :: Word8 } deriving (Show, Read, Eq, Ord)

data LargeRgb = LargeRgb { largeRed   :: Integer
                         , largeGreen :: Integer
                         , largeBlue  :: Integer } deriving (Show, Read, Eq, Ord)

toLarge :: Rgb -> LargeRgb
toLarge (Rgb r g b) = (LargeRgb (toInteger r) (toInteger g) (toInteger b))                                                             
                               
{-- instance Show Rgb where
    show (Rgb r g b) = show r ++ " " ++ show g ++ " " ++ show b
--}

data Pixel = Pixel { color   :: Rgb
                   , cluster :: Rgb
                   , distance  :: Integer } deriving (Show,Read)

data Image = Image { width   :: Int
                   , height  :: Int
                   , content :: [[Pixel]] } deriving (Show, Read)


{- function is used in saveImage -}
printCluster :: Pixel -> String
printCluster (Pixel _ (Rgb r g b) _) = show r ++ " " ++ show g ++ " " ++ show b

printColor :: Pixel -> String
printColor (Pixel (Rgb r g b) _ _) = show r ++ " " ++ show g ++ " " ++ show b

{-- actually, this function multiplies any Integral number, but returns Word8 --}
multWord8WithFrac :: (RealFrac a) => Word8 -> a -> Word8
multWord8WithFrac w n = round (fromIntegral w * n) :: Word8

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


{- FUNCTION FOR SAVING IMAGE TO FILE -}

saveImageClusters :: FilePath -> Image -> IO()
saveImageClusters = saveImage printCluster

saveImageColors :: FilePath -> Image -> IO()
saveImageColors = saveImage printColor

saveImage :: (Pixel -> String) -> FilePath -> Image -> IO()
saveImage whatToSave path img = do 
                            writeFile path header
                            appendFile path (maxColorValue ++ "\n")
                            appendFile path $ unlines (map whatToSave pixels) --lazy
                            where header        = "P3\n" ++ show (width img) ++ " " ++ show (height img) ++ "\n"
                                  maxColorValue = show 255
                                  pixels        = concat $ content img

getWidth :: [String] -> Int
getWidth ln = read (ln !! 0) :: Int

getHeight :: [String] -> Int
getHeight ln = read (ln !! 1) :: Int

extractRow :: (Eq t, Num t) => t -> [String] -> [Pixel]
extractRow 0 _ = []
extractRow _ [] = []
extractRow w lst@(r:g:b:rest) = (Pixel (Rgb (read r :: Word8) (read g :: Word8) (read b :: Word8))
                                        defaultCluster
                                        maxDist) : extractRow (w - 1) rest
                            where maxDist       = fromIntegral (maxBound :: Int)
                                  defaultCluster = (Rgb 0 0 0)

extractRow _ _ = error "Bad file structure!"

extractPixels :: (Eq t, Num t) => Int -> t -> [String] -> [[Pixel]]
extractPixels _ 0 _ = []
extractPixels w h lst = extractRow w lst : extractPixels w (h - 1) (drop (w * 3) lst)

parseImg :: [String] -> Image
parseImg (f:x:y:xs) 
    | f /= "P3" = error "Unknown format"
    | otherwise = (Image width height pixels)
                        where firstLineWords = (words x)
                              width = getWidth firstLineWords
                              height = getHeight firstLineWords
                              pixels = extractPixels width height $ words (unlines xs)

loadImage :: String -> IO Image
loadImage path = do contents <- readFile path
                    let linesOfFile = lines contents
                        img = parseImg linesOfFile
                    if (length (content img) /= (height img)) || (length (content img !! 0) /= (width img)) then error "Bad file structure"
                                                                                                            else return img