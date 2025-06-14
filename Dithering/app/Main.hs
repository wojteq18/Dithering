import Codec.Picture
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.Vector.Storable as V
import Control.Monad           (forM_, when)
import Control.Monad.ST        (runST)
import qualified Data.Vector.Storable.Mutable as M
import Data.List (minimumBy)
import Data.Word (Word8)

type PalletteFunc = Int -> (Double, Double, Double) -> (Double, Double, Double)

getIndex :: Int -> Int -> Int -> Int -> Int --zwroc indeks piksela (x,y) w wektorze pixeli
getIndex x y width c = (y * width + x) * 3 + c -- (y * szerokość + x) * 3, bo każdy piksel ma 3 wartości (R, G, B) c = 0, 1 lub 2 wybiera kanal RGB

clamp :: Double -> Double --ograniczenie wartości do zakresu [0, 255] do RGB
clamp x
    | x < 0     = 0
    | x > 255   = 255
    | otherwise = x

bayerMatrix4x4 :: [[Int]]
bayerMatrix4x4 =
  [ [ 0,  8,  2, 10],
    [12,  4, 14,  6],
    [ 3, 11,  1,  9],
    [15,  7, 13,  5]
  ]    

quantizeChannel :: Int -> Double -> Double 
quantizeChannel n value =
    let levels = (2^n) :: Int --liczba poziomów szarości
        step = 255 / fromIntegral (levels - 1) --krok między poziomami
        idx = round (value / step) :: Int --indeks poziomu
    in clamp (fromIntegral idx * step) --zwróć wartość zredukowaną do n bitów

quantizeChannelOrdered :: Int    --  Liczba bitów
                       -> Double --  Oryginalna wartość kanału (0-255)
                       -> Int    --  Wartość z macierzy Bayera
                       -> Int    --  Maksymalna wartość w macierzy Bayera + 1 
                       -> Double --  Wynikowa skwantyzowana wartość kanału
quantizeChannelOrdered nBits originalValue bayerVal matrixMaxValPlusOne =
    let numLevels = (2^nBits) :: Int
        step = if numLevels <= 1 then 255.0 else 255.0 / fromIntegral (numLevels - 1)
        thresholdAdjustment = ( (fromIntegral bayerVal / fromIntegral matrixMaxValPlusOne) - 0.5) * step --bayerVal to wartość z macierzy dla danego piksela
        --tresholdAdjustment to wartość znormalizowana do zakresu [-0.5, 0.5]
        
        modifiedValue = originalValue + thresholdAdjustment

        levelIndex = if numLevels <= 1 then 0 else round (clamp modifiedValue / step)
        quantizedValue = fromIntegral levelIndex * step
    in clamp quantizedValue 

orderedDitherVector bayerMatrix nBits width height vec = runST $ do
    mv <- V.thaw vec 

    let matrixH = length bayerMatrix
    let matrixW = if matrixH > 0 then length (head bayerMatrix) else 0
    let bayerMatrixMaxValPlusOne = matrixH * matrixW

    when (matrixH == 0 || matrixW == 0) $ error "Bayer matrix cannot be empty"

    forM_ [0 .. height - 1] $ \y ->
      forM_ [0 .. width  - 1] $ \x -> do
        -- Pobierz wartość z macierzy Bayera dla bieżącego piksela, zapętlając macierz
        let bayerVal = (bayerMatrix !! (y `mod` matrixH)) !! (x `mod` matrixW) --dostęp do wartości z macierzy Bayera

        -- Przetwarzanie kanału czerwonego 
        let idxR = getIndex width x y 0 
        oldR <- M.read mv idxR
        let newR = quantizeChannelOrdered nBits oldR bayerVal bayerMatrixMaxValPlusOne
        M.write mv idxR newR

        -- Przetwarzanie kanału zielonego 
        let idxG = getIndex width x y 1
        oldG <- M.read mv idxG
        let newG = quantizeChannelOrdered nBits oldG bayerVal bayerMatrixMaxValPlusOne
        M.write mv idxG newG

        -- Przetwarzanie kanału niebieskiego 
        let idxB = getIndex width x y 2
        oldB <- M.read mv idxB
        let newB = quantizeChannelOrdered nBits oldB bayerVal bayerMatrixMaxValPlusOne
        M.write mv idxB newB

    V.freeze mv

quantizeVector :: Int -> V.Vector Double -> V.Vector Double --zredukuj caly wektor wartości RGB do n bitów
quantizeVector nBits = V.map (quantizeChannel nBits)

-- Generowanie palety
generatePalette :: Int -> [(Double, Double, Double)]
generatePalette n =
  let levels = [0, 255 / ((2^n) - 1) .. 255]
  in [(r,g,b) | r <- levels, g <- levels, b <- levels]

-- Manhattan
findClosestManhattan :: (Double, Double, Double) -> [(Double, Double, Double)] -> (Double, Double, Double)
findClosestManhattan (r,g,b) = minimumBy (\(r1,g1,b1) (r2,g2,b2) -> compare (dist (r1,g1,b1)) (dist (r2,g2,b2)))
  where dist (r',g',b') = abs (r - r') + abs (g - g') + abs (b - b')

paletteFuncManhattan :: Int -> (Double, Double, Double) -> (Double, Double, Double)
paletteFuncManhattan n px = findClosestManhattan px (generatePalette n)

-- Euklidesowa (kanałowa)
paletteFuncEuklides :: Int -> (Double, Double, Double) -> (Double, Double, Double)
paletteFuncEuklides n (r,g,b) = (quantizeChannel n r, quantizeChannel n g, quantizeChannel n b)

-- ditherVector
ditherVector
  :: PalletteFunc -- ^ funkcja kwantyzująca: nBits -> (r,g,b) -> (r',g',b')
  -> Int              -- ^ liczba bitów
  -> Int              -- ^ szerokość
  -> Int              -- ^ wysokość
  -> V.Vector Double  -- ^ dane wejściowe
  -> V.Vector Double
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
        let errR = oldR - newR
            errG = oldG - newG
            errB = oldB - newB
            prop dx dy w = do
              let x' = x + dx
                  y' = y + dy
              when (x' >= 0 && x' < width && y' >= 0 && y' < height) $ do
                let iR = getIndex width x' y' 0
                    iG = getIndex width x' y' 1
                    iB = getIndex width x' y' 2
                vR <- M.read mv iR
                vG <- M.read mv iG
                vB <- M.read mv iB
                M.write mv iR (vR + errR * w)
                M.write mv iG (vG + errG * w)
                M.write mv iB (vB + errB * w)
        prop 1  0  (7/16)
        prop (-1) 1 (3/16)
        prop 0   1 (5/16)
        prop 1   1 (1/16)
    V.freeze mv

--atkinson
atkinsonVector    
  :: PalletteFunc -- ^ funkcja kwantyzująca: nBits -> (r,g,b) -> (r',g',b')
  -> Int              -- ^ liczba bitów
  -> Int              -- ^ szerokość
  -> Int              -- ^ wysokość
  -> V.Vector Double  -- ^ dane wejściowe
  -> V.Vector Double
atkinsonVector paletteFunc nBits width height vec = runST $ do
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
        let errR = oldR - newR
            errG = oldG - newG
            errB = oldB - newB
            prop dx dy w = do
              let x' = x + dx
                  y' = y + dy
              when (x' >= 0 && x' < width && y' >= 0 && y' < height) $ do
                let iR = getIndex width x' y' 0
                    iG = getIndex width x' y' 1
                    iB = getIndex width x' y' 2
                vR <- M.read mv iR
                vG <- M.read mv iG
                vB <- M.read mv iB
                M.write mv iR (vR + errR * w)
                M.write mv iG (vG + errG * w)
                M.write mv iB (vB + errB * w)
        prop 1  0  (1/8)
        prop (-1) 1 (1/8)
        prop 0   1 (1/8)
        prop 1   1 (1/8)
        prop 2   0 (1/8)
        prop 0  2 (1/8)
    V.freeze mv

vectorToImage :: Int -> Int -> V.Vector Double -> Image PixelRGB8
vectorToImage width height vecD =
    let vecW8 = V.map (\x -> fromIntegral (round (clamp x) :: Integer) :: Word8) vecD
    in Image width height vecW8

main :: IO ()
main = do
    args <- getArgs
    case args of
      [inPath, outRoot, bitsStr, metricStr] -> do
        let nBits = case reads bitsStr of
                        [(n, "")] | n > 0 && n <= 8 -> n
                        _ -> 3
            paletteFunc = case metricStr of
                "man" -> paletteFuncManhattan
                "euk" -> paletteFuncEuklides
                _     -> paletteFuncEuklides
        ei <- readImage inPath
        imgRGB <- case ei of
          Left err -> putStrLn ("Błąd przy wczytywaniu obrazu: " ++ err) >> exitFailure
          Right di -> pure (convertRGB8 di)

        let width  = imageWidth imgRGB
            height = imageHeight imgRGB
            raw8   = imageData imgRGB
            rawD   = V.map fromIntegral raw8

        let quantD  = quantizeVector nBits rawD
            quantImg = vectorToImage width height quantD
        writePng (outRoot ++ "_quant.png") quantImg

        let ditherD = ditherVector paletteFunc nBits width height rawD
            ditherImg = vectorToImage width height ditherD
        writePng (outRoot ++ "_dither.png") ditherImg

        let atkinsonD = atkinsonVector paletteFunc nBits width height rawD
            atkinsonImg = vectorToImage width height atkinsonD
        writePng (outRoot ++ "_atkinson.png") atkinsonImg

        let orderedD = orderedDitherVector bayerMatrix4x4 nBits width height rawD
            orderedImg = vectorToImage width height orderedD
        writePng (outRoot ++ "_ordered.png") orderedImg

        --putStrLn $ "zapisano: " ++ outRoot ++ "_quant.png " ++ outRoot ++ "_dither.png"

      [inPath, outRoot, bitsStr] -> do -- domyślnie euklidesowa
        let nBits = case reads bitsStr of
                        [(n, "")] | n > 0 && n <= 8 -> n
                        _ -> 3
            paletteFunc = paletteFuncEuklides
        ei <- readImage inPath
        imgRGB <- case ei of
          Left err -> putStrLn ("Błąd przy wczytywaniu obrazu: " ++ err) >> exitFailure
          Right di -> pure (convertRGB8 di)

        let width  = imageWidth imgRGB
            height = imageHeight imgRGB
            raw8   = imageData imgRGB
            rawD   = V.map fromIntegral raw8

        let quantD  = quantizeVector nBits rawD
            quantImg = vectorToImage width height quantD
        writePng (outRoot ++ "_quant.png") quantImg

        let ditherD = ditherVector paletteFunc nBits width height rawD
            ditherImg = vectorToImage width height ditherD
        writePng (outRoot ++ "_dither.png") ditherImg



        putStrLn $ "zapisano: " ++ outRoot ++ "_quant.png " ++ outRoot ++ "_dither.png"

      [inPath, outRoot] -> do -- domyślnie euklidesowa i 3 bity
        let nBits = 3
            paletteFunc = paletteFuncEuklides
        ei <- readImage inPath
        imgRGB <- case ei of
          Left err -> putStrLn ("Błąd przy wczytywaniu obrazu: " ++ err) >> exitFailure
          Right di -> pure (convertRGB8 di)

        let width  = imageWidth imgRGB
            height = imageHeight imgRGB
            raw8   = imageData imgRGB
            rawD   = V.map fromIntegral raw8

        let quantD  = quantizeVector nBits rawD
            quantImg = vectorToImage width height quantD
        writePng (outRoot ++ "_quant.png") quantImg

        let ditherD = ditherVector paletteFunc nBits width height rawD
            ditherImg = vectorToImage width height ditherD
        writePng (outRoot ++ "_dither.png") ditherImg

        --putStrLn $ "zapisano: " ++ outRoot ++ "_quant.png " ++ outRoot ++ "_dither.png"

        let atkinsonD = atkinsonVector paletteFunc nBits width height rawD
            atkinsonImg = vectorToImage width height atkinsonD
        writePng (outRoot ++ "_atkinson.png") atkinsonImg    

        let orderedD = orderedDitherVector bayerMatrix4x4 nBits width height rawD
            orderedImg = vectorToImage width height orderedD
        writePng (outRoot ++ "_ordered.png") orderedImg

      _ -> do
        putStrLn "uzycie: dither input.png output-root [nBits] [euk|man]"
        exitFailure
