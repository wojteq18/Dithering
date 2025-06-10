import Codec.Picture 
import Codec.Picture.Types
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.Vector.Storable as V
import Control.Monad           (forM_, when)
import Control.Monad.ST        (runST, ST)
import qualified Data.Vector.Storable.Mutable as M

type PaletteFunc = Int -> Double -> Double

getIndex :: Int -> Int -> Int -> Int -> Int --zwroc indeks piksela (x,y) w wektorze pixeli
getIndex x y width c = (y * width + x) * 3 + c -- (y * szerokość + x) * 3, bo każdy piksel ma 3 wartości (R, G, B) c = 0, 1 lub 2 wybiera kanal RGB

clamp :: Double -> Double --ograniczenie wartości do zakresu [0, 255] do RGB
clamp x
    | x < 0     = 0
    | x > 255   = 255
    | otherwise = x

pixelToDouble :: PixelRGB8 -> (Double, Double, Double) --zamiana piksela na trojkę wartości RGB jako Double
pixelToDouble (PixelRGB8 r g b) = (fromIntegral r, fromIntegral g, fromIntegral b)

doubleToPixel :: (Double, Double, Double) -> PixelRGB8 --odwrotnie
doubleToPixel (r, g, b) = PixelRGB8 (toWord8 r) (toWord8 g) (toWord8 b)
    where toWord8 = fromIntegral . round . clamp

quantizeChannel :: Int -> Double -> Double --zmiana wartości kanału RGB na wartość zredukowaną do n bitów
quantizeChannel n value = 
    let levels = 2 ^ n --liczba poziomów szarości
        step = 255 / fromIntegral (levels - 1) --krok między poziomami
        idx = round (value / step) --indeks poziomu
    in clamp (fromIntegral idx * step) --zwróć wartość zredukowaną do n bitów

quantizePixel :: Int -> (Double, Double, Double) -> (Double, Double, Double) --zastosuj redukcję do każdego kanału RGB
quantizePixel n (r, g, b) = 
    (quantizeChannel n r, quantizeChannel n g, quantizeChannel n b)


quantizeVector :: Int -> V.Vector Double -> V.Vector Double --zredukuj caly wektor wartości RGB do n bitów
quantizeVector nBits = V.map (quantizeChannel nBits)

ditherVector :: PaletteFunc -> Int -> Int -> Int -> V.Vector Double -> V.Vector Double -- floyd-steinberg dithering
ditherVector paletteFunc nBits width height vec = runST $ do
    mv <- V.thaw vec
    forM_ [0 .. height - 1] $ \y ->
      forM_ [0 .. width  - 1] $ \x ->
        forM_ [0..2] $ \c -> do
          let idx = getIndex width x y c
          old <- M.read mv idx
          let new = paletteFunc nBits old
          M.write mv idx new
          let err = old - new
              prop dx dy w =
                let x' = x + dx
                    y' = y + dy
                in when (x' >= 0 && x' < width && y' >= 0 && y' < height) $ do
                     let idx' = getIndex width x' y' c
                     v' <- M.read mv idx'
                     M.write mv idx' (v' + err * w)
          prop 1  0  (7/16)
          prop (-1) 1 (3/16)
          prop 0   1 (5/16)
          prop 1   1 (1/16)
    V.freeze mv

vectorToImage :: Int -> Int -> V.Vector Double -> Image PixelRGB8 --konwersja wektora wartości RGB do obrazu
vectorToImage width height vecD =
    let vecW8 = V.map (fromIntegral . round . clamp) vecD
    in Image width height vecW8

main :: IO ()
main = do
    args <- getArgs
    case args of
      [inPath, outRoot, bitsStr] -> do
        let nBits = read bitsStr
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

        let ditherD  = ditherVector quantizeChannel nBits width height rawD
            ditherImg = vectorToImage width height ditherD
        writePng (outRoot ++ "_dither.png") ditherImg

        putStrLn $ "zapisano: " ++ outRoot ++ "_quant.png " ++ outRoot ++ "_dither.png"

      _ -> do
        putStrLn "uzycie: dither input.png output-root nBits"
        exitFailure
