-- app-cli/Main.hs
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import Codec.Picture (readImage, writePng, convertRGB8)
-- The import of ‘System.FilePath’ was redundant.

-- Import our core dithering logic!
import Dithering.Core

main :: IO ()
main = do
    args <- getArgs
    if "-visual" `elem` args
    then putStrLn "Visual mode not supported in CLI version. Run the 'dithering-gui' executable."
    else runCli args

runCli :: [String] -> IO ()
runCli args = do
    (inPath, outRoot, nBits, metric) <- case args of
      [i, o, b, m] -> pure (i, o, parseBits b, parseMetric m)
      [i, o, b]    -> pure (i, o, parseBits b, Euclidean)
      [i, o]       -> pure (i, o, 3, Euclidean)
      _            -> usage >> exitFailure

    eImg <- readImage inPath
    case eImg of
      Left err -> putStrLn ("Error loading image: " ++ err) >> exitFailure
      Right dImg -> do
        let imgRGB = convertRGB8 dImg
            (width, height) = getImageDimensions imgRGB
            rawD = imageToDoubleVector imgRGB

        putStrLn $ "Processing with " ++ show nBits ++ " bits and " ++ show metric ++ " metric..."

        let processAndSave algo name = do
              let resultD = applyDithering algo metric nBits width height rawD
                  resultImg = vectorToImage width height resultD
                  outPath = outRoot ++ "_" ++ name ++ ".png"
              writePng outPath resultImg
              putStrLn $ "Saved: " ++ outPath

        processAndSave SimpleQuantization "quant"
        processAndSave FloydSteinberg "dither_fs"
        processAndSave Atkinson "dither_atkinson"
        processAndSave Ordered "dither_ordered"
        
        exitSuccess

usage :: IO ()
usage = putStrLn "Usage: dithering-cli input.png output-root [nBits] [euk|man]"

parseBits :: String -> Int
parseBits s = case reads s of
                [(n, "")] | n > 0 && n <= 8 -> n
                _ -> 3 -- Default

parseMetric :: String -> PaletteMetric
parseMetric "man" = Manhattan
parseMetric _     = Euclidean