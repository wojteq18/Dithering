import Codec.Picture 
import Codec.Picture.Types
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.Vector.Storable as V
import Data.Bits ((.&.), shiftR, shiftL)


-- Ogranicz wartość kanału do n bitów
reduceBits :: Int -> Pixel8 -> Pixel8
reduceBits n x =
    let shift = 8 - n
        mask = (0xFF `shiftR` shift) `shiftL` shift -- Tworzy maskę z n najważniejszymi bitami i zeruje resztę, ograniczając wartość kanału do n bitów
    in (x .&. mask)

-- Przetwórz obraz, ograniczając liczbę bitów na kanał
reduceImageBits :: Int -> Image PixelRGB8 -> Image PixelRGB8
reduceImageBits n img = pixelMap reducePixel img --pixelMap aplikuje transformację do wszystkich pixeli obrazu
    where
        reducePixel (PixelRGB8 r g b) = PixelRGB8 (reduceBits n r) (reduceBits n g) (reduceBits n b)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputPath, outputPath, bitsStr] -> do
            let nBits = read bitsStr :: Int --zmiana na int
            result <- readImage inputPath --readImage :: FilePath -> IO (Either String DynamicImage) - lewa strona to błąd, prawa strona to obraz

            case result of
                Left err -> do
                    putStrLn $ "Błąd przy wczytywaniu obrazu: " ++ err
                    exitFailure
                Right img -> do
                    let rgbImg = convertRGB8 img
                    let reducedImg = reduceImageBits nBits rgbImg -- nBits bitow na kanał
                    let Image w h dat = reducedImg --dat :: Vector Word8 - dane RGB po kolei: R, G, B, R, G ..., długość 3 * w * h
                    putStrLn $ "Szerokość: " ++ show w ++ ", Wysokość: " ++ show h
                    let x = 10
                    let y = 10
                    let pixelIndex = (y * w + x) * 3 -- (y * szerokość + x) * 3, bo każdy piksel ma 3 wartości (R, G, B)
                    let r = dat V.! pixelIndex
                    let g = dat V.! (pixelIndex + 1)
                    let b = dat V.! (pixelIndex + 2)
                    putStrLn $ "Kolor piksela na pozycji (10, 10): R=" ++ show r ++ ", G=" ++ show g ++ ", B=" ++ show b
                    --tutaj moment zapisu do outputu
                    let newImage = Image w h dat :: Image PixelRGB8 
                    writePng outputPath newImage -- zapis obrazu do pliku PNG
        _ -> do
            exitFailure
