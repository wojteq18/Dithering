import Codec.Picture (readImage, DynamicImage)
import System.Environment (getArgs)
import System.Exit (exitFailure)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [filePath] -> do
            result <- readImage filePath --readImage :: FilePath -> IO (Either String DynamicImage) - lewa strona to błąd, prawa strona to obraz

            case result of
                Left err -> do
                    putStrLn $ "Błąd przy wczytywaniu obrazu: " ++ err
                    exitFailure
                Right img -> do
                    putStrLn "Obraz wczytany poprawnie"
        _ -> do
            exitFailure
