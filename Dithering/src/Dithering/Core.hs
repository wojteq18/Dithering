-- src/Dithering/Core.hs
module Dithering.Core
    ( -- Types
      DitheringAlgorithm(..)
    , PaletteMetric(..)
    , ImageRGB -- Re-exporting for convenience
      -- Core Functions
    , applyDithering
    , vectorToImage
    , imageToDoubleVector
    , getImageDimensions
    ) where

-- Removed 'convertRGB8' from the import list as it was unused.
import Codec.Picture (Image(Image), PixelRGB8, imageWidth, imageHeight, imageData)
import qualified Data.Vector.Storable as V
import Control.Monad (forM_, when)
import Control.Monad.ST (runST)
import qualified Data.Vector.Storable.Mutable as M
import Data.List (minimumBy)
import Data.Word (Word8)

-- Types are unchanged...
type ImageRGB = Image PixelRGB8
type Pixel = (Double, Double, Double)
type PaletteFunc = Int -> Pixel -> Pixel
data DitheringAlgorithm = FloydSteinberg | Atkinson | Ordered | SimpleQuantization deriving (Show, Eq)
data PaletteMetric = Euclidean | Manhattan deriving (Show, Eq)

-- Top-level functions are unchanged...
applyDithering :: DitheringAlgorithm -> PaletteMetric -> Int -> Int -> Int -> V.Vector Double -> V.Vector Double
applyDithering SimpleQuantization _ nBits _ _ vec = quantizeVector nBits vec
applyDithering FloydSteinberg metric nBits width height vec = ditherVector (paletteFuncFor metric) nBits width height vec
applyDithering Atkinson metric nBits width height vec = atkinsonVector (paletteFuncFor metric) nBits width height vec
applyDithering Ordered _ nBits width height vec = orderedDitherVector bayerMatrix4x4 nBits width height vec
getImageDimensions :: ImageRGB -> (Int, Int)
getImageDimensions img = (imageWidth img, imageHeight img)
imageToDoubleVector :: ImageRGB -> V.Vector Double
imageToDoubleVector = V.map fromIntegral . imageData
vectorToImage :: Int -> Int -> V.Vector Double -> ImageRGB
vectorToImage width height vecD =
    let vecW8 = V.map (\x -> fromIntegral (round (clamp x) :: Integer) :: Word8) vecD
    in Image width height vecW8

-- Helper and algorithm implementations...
paletteFuncFor :: PaletteMetric -> PaletteFunc
paletteFuncFor Euclidean = paletteFuncEuklides
paletteFuncFor Manhattan = paletteFuncManhattan
getIndex :: Int -> Int -> Int -> Int -> Int
getIndex width x y c = (y * width + x) * 3 + c
clamp :: Double -> Double
clamp x | x < 0 = 0 | x > 255 = 255 | otherwise = x
bayerMatrix4x4 :: [[Int]]
bayerMatrix4x4 = [ [ 0,  8,  2, 10], [12,  4, 14,  6], [ 3, 11,  1,  9], [15,  7, 13,  5] ]
quantizeChannel :: Int -> Double -> Double 
quantizeChannel n value =
    let levels = (2^n) :: Int
        step = 255 / fromIntegral (levels - 1)
        idx = round (value / step) :: Int
    in clamp (fromIntegral idx * step)

-- **Fix for the type ambiguity warning is here**
quantizeChannelOrdered :: Int -> Double -> Int -> Int -> Double
quantizeChannelOrdered nBits originalValue bayerVal matrixMaxValPlusOne =
    let numLevels = (2^nBits) :: Int
        step = if numLevels <= 1 then 255.0 else 255.0 / fromIntegral (numLevels - 1)
        thresholdAdjustment = ( (fromIntegral bayerVal / fromIntegral matrixMaxValPlusOne) - 0.5) * step
        modifiedValue = originalValue + thresholdAdjustment
        -- By adding `:: Int`, we tell the compiler exactly which type `levelIndex` should be.
        levelIndex = if numLevels <= 1 then 0 else round (clamp modifiedValue / step) :: Int
        quantizedValue = fromIntegral levelIndex * step
    in clamp quantizedValue

quantizeVector :: Int -> V.Vector Double -> V.Vector Double
quantizeVector nBits = V.map (quantizeChannel nBits)
generatePalette :: Int -> [Pixel]
generatePalette n = let levels = [0, 255 / ((2^n) - 1) .. 255] in [(r,g,b) | r <- levels, g <- levels, b <- levels]
findClosestManhattan :: Pixel -> [Pixel] -> Pixel
findClosestManhattan (r,g,b) = minimumBy (\(r1,g1,b1) (r2,g2,b2) -> compare (dist (r1,g1,b1)) (dist (r2,g2,b2)))
  where dist (r',g',b') = abs (r - r') + abs (g - g') + abs (b - b')
paletteFuncManhattan :: PaletteFunc
paletteFuncManhattan n px = findClosestManhattan px (generatePalette n)
paletteFuncEuklides :: PaletteFunc
paletteFuncEuklides n (r,g,b) = (quantizeChannel n r, quantizeChannel n g, quantizeChannel n b)

-- All other functions are unchanged...
orderedDitherVector :: [[Int]] -> Int -> Int -> Int -> V.Vector Double -> V.Vector Double
orderedDitherVector bayerMatrix nBits width height vec = runST $ do
    mv <- V.thaw vec
    let matrixH = length bayerMatrix
    let matrixW = if matrixH > 0 then length (head bayerMatrix) else 0
    let bayerMatrixMaxValPlusOne = matrixH * matrixW
    when (matrixH == 0 || matrixW == 0) $ error "Bayer matrix cannot be empty"
    forM_ [0 .. height - 1] $ \y ->
      forM_ [0 .. width  - 1] $ \x -> do
        let bayerVal = (bayerMatrix !! (y `mod` matrixH)) !! (x `mod` matrixW)
        let idxR = getIndex width x y 0
        oldR <- M.read mv idxR
        M.write mv idxR (quantizeChannelOrdered nBits oldR bayerVal bayerMatrixMaxValPlusOne)
        let idxG = getIndex width x y 1
        oldG <- M.read mv idxG
        M.write mv idxG (quantizeChannelOrdered nBits oldG bayerVal bayerMatrixMaxValPlusOne)
        let idxB = getIndex width x y 2
        oldB <- M.read mv idxB
        M.write mv idxB (quantizeChannelOrdered nBits oldB bayerVal bayerMatrixMaxValPlusOne)
    V.freeze mv
ditherVector :: PaletteFunc -> Int -> Int -> Int -> V.Vector Double -> V.Vector Double
ditherVector paletteFunc nBits width height vec = runST $ do
    mv <- V.thaw vec
    forM_ [0 .. height - 1] $ \y ->
      forM_ [0 .. width  - 1] $ \x -> do
        let idxR = getIndex width x y 0
            idxG = getIndex width x y 1
            idxB = getIndex width x y 2
        oldR <- M.read mv idxR
        oldG <- M.read mv idxG
        oldB <- M.read mv idxB
        let (newR, newG, newB) = paletteFunc nBits (oldR, oldG, oldB)
        M.write mv idxR newR
        M.write mv idxG newG
        M.write mv idxB newB
        let errR = oldR - newR; errG = oldG - newG; errB = oldB - newB
            prop dx dy w = do
              let x' = x + dx; y' = y + dy
              when (x' >= 0 && x' < width && y' >= 0 && y' < height) $ do
                let iR = getIndex width x' y' 0; iG = getIndex width x' y' 1; iB = getIndex width x' y' 2
                vR <- M.read mv iR; vG <- M.read mv iG; vB <- M.read mv iB
                M.write mv iR (vR + errR * w); M.write mv iG (vG + errG * w); M.write mv iB (vB + errB * w)
        prop 1 0 (7/16); prop (-1) 1 (3/16); prop 0 1 (5/16); prop 1 1 (1/16)
    V.freeze mv
atkinsonVector :: PaletteFunc -> Int -> Int -> Int -> V.Vector Double -> V.Vector Double
atkinsonVector paletteFunc nBits width height vec = runST $ do
    mv <- V.thaw vec
    forM_ [0 .. height - 1] $ \y ->
      forM_ [0 .. width  - 1] $ \x -> do
        let idxR = getIndex width x y 0; idxG = getIndex width x y 1; idxB = getIndex width x y 2
        oldR <- M.read mv idxR; oldG <- M.read mv idxG; oldB <- M.read mv idxB
        let (newR, newG, newB) = paletteFunc nBits (oldR, oldG, oldB)
        M.write mv idxR newR; M.write mv idxG newG; M.write mv idxB newB
        let errR = oldR - newR; errG = oldG - newG; errB = oldB - newB
            prop dx dy w = do
              let x' = x + dx; y' = y + dy
              when (x' >= 0 && x' < width && y' >= 0 && y' < height) $ do
                let iR = getIndex width x' y' 0; iG = getIndex width x' y' 1; iB = getIndex width x' y' 2
                vR <- M.read mv iR; vG <- M.read mv iG; vB <- M.read mv iB
                M.write mv iR (vR + errR * w); M.write mv iG (vG + errG * w); M.write mv iB (vB + errB * w)
        prop 1 0 (1/8); prop (-1) 1 (1/8); prop 0 1 (1/8); prop 1 1 (1/8); prop 2 0 (1/8); prop 0 2 (1/8)
    V.freeze mv