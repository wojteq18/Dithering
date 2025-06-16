-- app-gui/Gui.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

import qualified GI.Gtk as Gtk
import qualified GI.GdkPixbuf as GdkPixbuf
import qualified GI.GLib as GLib
import Data.GI.Base
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO)
import Control.Concurrent.STM
import Data.Maybe (isJust, fromJust) -- Removed 'maybe' as it's not needed
import qualified Data.Vector.Storable as V
import qualified Data.ByteString as BS

import Codec.Picture (readImage, writePng, convertRGB8, imageWidth, imageHeight, imageData)
import Dithering.Core

-- This is the final, simplified, and correct version of the conversion function.
imageRGBToPixbuf :: ImageRGB -> IO GdkPixbuf.Pixbuf
imageRGBToPixbuf img = do
    let vec = imageData img
        byteString = BS.pack $ V.toList vec
    bytes <- GLib.bytesNew (Just byteString)

    -- **THE FIX IS HERE:** We now assume this function returns `IO GdkPixbuf.Pixbuf`
    -- and throws an exception on failure, as dictated by the compiler error.
    -- We call it as the last statement in the 'do' block, and its result
    -- becomes the result of the entire function.
    GdkPixbuf.pixbufNewFromBytes bytes GdkPixbuf.ColorspaceRgb False 8
                (fromIntegral $ imageWidth img)
                (fromIntegral $ imageHeight img)
                (fromIntegral $ imageWidth img * 3)

-- State definition
data AppState = AppState
    { originalImage  :: Maybe ImageRGB
    , currentAlgo    :: DitheringAlgorithm
    , currentMetric  :: PaletteMetric
    , currentNBits   :: Int
    }

-- Initial state
initialState :: AppState
initialState = AppState
    { originalImage  = Nothing
    , currentAlgo    = FloydSteinberg
    , currentMetric  = Euclidean
    , currentNBits   = 3
    }

main :: IO ()
main = do
    void $ Gtk.init Nothing
    appState <- newTVarIO initialState

    win <- new Gtk.Window [#title := "Haskell Dithering"]; void $ on win #destroy Gtk.mainQuit; #resize win 800 600
    grid <- new Gtk.Grid [#columnSpacing := 10, #rowSpacing := 10, #margin := 10]
    imgDisplay <- new Gtk.Image []; #setHexpand imgDisplay True; #setVexpand imgDisplay True; Gtk.gridAttach grid imgDisplay 0 0 1 10
    controlsBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 15]; #setHexpand controlsBox False; Gtk.gridAttach grid controlsBox 1 0 1 1
    fileBtn <- new Gtk.FileChooserButton [#title := "Choose an Image", #action := Gtk.FileChooserActionOpen]; Gtk.boxPackStart controlsBox fileBtn False False 5
    algoLabel <- new Gtk.Label [#label := "Algorithm"]; Gtk.boxPackStart controlsBox algoLabel False False 0
    algoCombo <- new Gtk.ComboBoxText []; Gtk.comboBoxTextAppendText algoCombo "FloydSteinberg"; Gtk.comboBoxTextAppendText algoCombo "Atkinson"; Gtk.comboBoxTextAppendText algoCombo "Ordered"; Gtk.comboBoxTextAppendText algoCombo "SimpleQuantization"; #setActive algoCombo 0; Gtk.boxPackStart controlsBox algoCombo False False 5
    metricLabel <- new Gtk.Label [#label := "Palette Metric"]; Gtk.boxPackStart controlsBox metricLabel False False 0
    metricCombo <- new Gtk.ComboBoxText []; Gtk.comboBoxTextAppendText metricCombo "Euclidean"; Gtk.comboBoxTextAppendText metricCombo "Manhattan"; #setActive metricCombo 0; Gtk.boxPackStart controlsBox metricCombo False False 5
    bitsLabel <- new Gtk.Label [#label := "Bits per channel (1-8)"]; Gtk.boxPackStart controlsBox bitsLabel False False 0
    bitsScale <- new Gtk.Scale [#orientation := Gtk.OrientationHorizontal, #drawValue := True]; #setRange bitsScale 1 8; #setValue bitsScale 3; #setDigits bitsScale 0; Gtk.boxPackStart controlsBox bitsScale False False 5
    saveBtn <- new Gtk.Button [#label := "Save Dithered Image..."]; Gtk.boxPackStart controlsBox saveBtn False False 10
    #add win grid
    
    displayedImage <- newTVarIO Nothing :: IO (TVar (Maybe ImageRGB))

    let processAndDisplay = do
          state <- readTVarIO appState
          case originalImage state of
            Nothing -> return ()
            Just img -> void . forkIO $ do
              let (w, h) = getImageDimensions img
                  rawD   = imageToDoubleVector img
              let ditheredD = applyDithering (currentAlgo state) (currentMetric state) (currentNBits state) w h rawD
              let resultImg = vectorToImage w h ditheredD
              pixbuf <- imageRGBToPixbuf resultImg
              atomically $ writeTVar displayedImage (Just resultImg)
              void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
                #setFromPixbuf imgDisplay (Just pixbuf)
                return False

    void $ on fileBtn #selectionChanged $ do
        mFile <- #getFilename fileBtn
        case mFile of
            Nothing -> return ()
            Just path -> do
                eImg <- liftIO $ readImage path
                case eImg of
                    Left err   -> putStrLn $ "Error loading image: " ++ err
                    Right dImg -> do
                        let img = convertRGB8 dImg
                        atomically $ modifyTVar' appState (\s -> s { originalImage = Just img })
                        processAndDisplay
    
    void $ on algoCombo #changed $ do
        mText <- Gtk.comboBoxTextGetActiveText algoCombo
        case mText of
            Just "FloydSteinberg"   -> atomically $ modifyTVar' appState (\s -> s { currentAlgo = FloydSteinberg })
            Just "Atkinson"          -> atomically $ modifyTVar' appState (\s -> s { currentAlgo = Atkinson })
            Just "Ordered"           -> atomically $ modifyTVar' appState (\s -> s { currentAlgo = Ordered })
            Just "SimpleQuantization"-> atomically $ modifyTVar' appState (\s -> s { currentAlgo = SimpleQuantization })
            _                        -> return ()
        processAndDisplay

    void $ on metricCombo #changed $ do
        mText <- Gtk.comboBoxTextGetActiveText metricCombo
        case mText of
            Just "Euclidean" -> atomically $ modifyTVar' appState (\s -> s { currentMetric = Euclidean })
            Just "Manhattan" -> atomically $ modifyTVar' appState (\s -> s { currentMetric = Manhattan })
            _                -> return ()
        processAndDisplay

    void $ on bitsScale #valueChanged $ do
        val <- round <$> #getValue bitsScale
        atomically $ modifyTVar' appState (\s -> s { currentNBits = val })
        processAndDisplay

    void $ on saveBtn #clicked $ do
        mImg <- readTVarIO displayedImage
        when (isJust mImg) $ do
            dialog <- new Gtk.FileChooserDialog [ #title := "Save File", #action := Gtk.FileChooserActionSave ]
            _ <- Gtk.dialogAddButton dialog "Cancel" (fromIntegral $ fromEnum Gtk.ResponseTypeCancel)
            _ <- Gtk.dialogAddButton dialog "Save"   (fromIntegral $ fromEnum Gtk.ResponseTypeAccept)
            #setTransientFor dialog (Just win) 
            response <- #run dialog
            when (toEnum (fromIntegral response) == Gtk.ResponseTypeAccept) $ do
                mFile <- #getFilename dialog
                case mFile of
                    Nothing -> return ()
                    Just path -> liftIO $ writePng path (fromJust $ mImg)
            #destroy dialog

    #showAll win
    Gtk.main