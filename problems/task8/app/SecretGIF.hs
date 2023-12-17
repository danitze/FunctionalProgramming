module Main where

import Graphics.GD
import Data.Bits
import Data.Char (ord)
import Data.Word (Word8)

-- Convert a character to a list of bits
charToBits :: Char -> [Bool]
charToBits c = map (testBit $ ord c) [0..7]

-- Convert a string to a list of bits
toBinary :: String -> [Bool]
toBinary = concatMap charToBits

-- Modify the least significant bit of a color component
modifyLSB :: Word8 -> Bool -> Word8
modifyLSB color bit = if bit then color `setBit` 0 else color `clearBit` 0

-- Modify the LSB of a pixel based on the bit
modifyPixel :: Pixel -> Bool -> Pixel
modifyPixel pixel bit =
    let (r, g, b, a) = rgbaOfColor pixel
        newR = modifyLSB r bit
    in colorRGBA newR g b a

hideInformationInImage :: Image -> [Bool] -> IO Image
hideInformationInImage img bits = do
    size <- imageSize img
    let positions = [(x, y) | y <- [0..snd size - 1], x <- [0..fst size - 1]]
        modifiedPixels = zip positions bits
    mapM_ (\((x, y), bit) -> do
              pixel <- getPixel (x, y) img
              let newPixel = modifyPixel pixel bit
              setPixel (x, y) newPixel img
          ) modifiedPixels
    return img

hideInformation :: FilePath -> String -> FilePath -> IO ()
hideInformation inputImagePath secretMessage outputImagePath = do
    image <- loadGifFile inputImagePath
    let binaryMessage = toBinary secretMessage
    modifiedImage <- hideInformationInImage image binaryMessage
    saveGifFile outputImagePath modifiedImage

main :: IO ()
main = do
    let inputImagePath = "input.gif"
    let secretMessage = "Secret"
    let outputImagePath = "output.gif"
    hideInformation inputImagePath secretMessage outputImagePath
