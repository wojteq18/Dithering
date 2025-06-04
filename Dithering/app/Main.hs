import Codec.Picture 
import Codec.Picture.Types
import System.Environment (getArgs)
import System.Exit (exitFailure)
import qualified Data.Vector.Storable as V
main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputPath, outputPath] -> do
            result <- readImage inputPath --readImage :: FilePath -> IO (Either String DynamicImage) - lewa strona to błąd, prawa strona to obraz

            case result of
                Left err -> do
                    putStrLn $ "Błąd przy wczytywaniu obrazu: " ++ err
                    exitFailure
                Right img -> do
                    let Image w h dat = convertRGB8 img --dat :: Vector Word8 - dane RGB po kolei: R, G, B, R, G ..., długość 3 * w * h
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
