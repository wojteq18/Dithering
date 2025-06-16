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
import Data.Maybe (isJust, fromJust)
import qualified Data.Vector.Storable as V
import qualified Data.ByteString as BS

import Codec.Picture (readImage, writePng, convertRGB8, imageWidth, imageHeight, imageData)
import Dithering.Core

-- This conversion function is correct and remains unchanged.
imageRGBToPixbuf :: ImageRGB -> IO GdkPixbuf.Pixbuf
imageRGBToPixbuf img = do
    let vec = imageData img
        byteString = BS.pack $ V.toList vec
    bytes <- GLib.bytesNew (Just byteString)
    GdkPixbuf.pixbufNewFromBytes bytes GdkPixbuf.ColorspaceRgb False 8
                (fromIntegral $ imageWidth img)
                (fromIntegral $ imageHeight img)
                (fromIntegral $ imageWidth img * 3)

-- State definition is unchanged.
data AppState = AppState
    { originalImage  :: Maybe ImageRGB
    , currentAlgo    :: DitheringAlgorithm
    , currentMetric  :: PaletteMetric
    , currentNBits   :: Int
    }

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
    
    -- UI controls are mostly the same
    fileBtn <- new Gtk.FileChooserButton [#title := "Choose an Image", #action := Gtk.FileChooserActionOpen]; Gtk.boxPackStart controlsBox fileBtn False False 5
    algoLabel <- new Gtk.Label [#label := "Algorithm"]; Gtk.boxPackStart controlsBox algoLabel False False 0
    algoCombo <- new Gtk.ComboBoxText []; Gtk.comboBoxTextAppendText algoCombo "FloydSteinberg"; Gtk.comboBoxTextAppendText algoCombo "Atkinson"; Gtk.comboBoxTextAppendText algoCombo "Ordered"; Gtk.comboBoxTextAppendText algoCombo "SimpleQuantization"; #setActive algoCombo 0; Gtk.boxPackStart controlsBox algoCombo False False 5
    metricLabel <- new Gtk.Label [#label := "Palette Metric"]; Gtk.boxPackStart controlsBox metricLabel False False 0
    metricCombo <- new Gtk.ComboBoxText []; Gtk.comboBoxTextAppendText metricCombo "Euclidean"; Gtk.comboBoxTextAppendText metricCombo "Manhattan"; #setActive metricCombo 0; Gtk.boxPackStart controlsBox metricCombo False False 5
    bitsLabel <- new Gtk.Label [#label := "Bits per channel (1-8)"]; Gtk.boxPackStart controlsBox bitsLabel False False 0
    bitsScale <- new Gtk.Scale [#orientation := Gtk.OrientationHorizontal, #drawValue := True]; #setRange bitsScale 1 8; #setValue bitsScale 3; #setDigits bitsScale 0; Gtk.boxPackStart controlsBox bitsScale False False 5
    
    -- NEW: The "Dither!" button. It starts disabled.
    runBtn <- new Gtk.Button [#label := "Dither!", #sensitive := False]
    Gtk.boxPackStart controlsBox runBtn False False 10

    saveBtn <- new Gtk.Button [#label := "Save Dithered Image..."]; Gtk.boxPackStart controlsBox saveBtn False False 10
    
    #add win grid
    
    displayedImage <- newTVarIO Nothing :: IO (TVar (Maybe ImageRGB))

    -- CHANGED: We now have two separate actions.
    
    -- This action runs the dithering algorithm based on the current state.
    let runDithering = do
          state <- readTVarIO appState
          case originalImage state of
            Nothing -> return () -- This shouldn't happen if the button is disabled correctly.
            Just img -> void . forkIO $ do
              putStrLn "Dithering process started..." -- For debugging
              let (w, h) = getImageDimensions img
                  rawD   = imageToDoubleVector img
              let ditheredD = applyDithering (currentAlgo state) (currentMetric state) (currentNBits state) w h rawD
              let resultImg = vectorToImage w h ditheredD
              pixbuf <- imageRGBToPixbuf resultImg
              atomically $ writeTVar displayedImage (Just resultImg)
              void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ do
                #setFromPixbuf imgDisplay (Just pixbuf)
                putStrLn "Dithering process finished, display updated." -- For debugging
                return False

    -- This action displays the original image and enables the "Dither!" button.
    let displayOriginalImage img = do
          pixbuf <- imageRGBToPixbuf img
          #setFromPixbuf imgDisplay (Just pixbuf)
          #setSensitive runBtn True

    -- CHANGED: The file button now ONLY loads and displays the original image.
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
                        -- Put the original image in the state
                        atomically $ modifyTVar' appState (\s -> s { originalImage = Just img })
                        -- Immediately display it
                        displayOriginalImage img
    
    -- NEW: Connect the "Dither!" button to our runDithering action.
    void $ on runBtn #clicked runDithering

    -- The other event handlers just update the state. They don't trigger dithering directly.
    void $ on algoCombo #changed $ do
        mText <- Gtk.comboBoxTextGetActiveText algoCombo
        case mText of
            Just "FloydSteinberg"   -> atomically $ modifyTVar' appState (\s -> s { currentAlgo = FloydSteinberg })
            Just "Atkinson"          -> atomically $ modifyTVar' appState (\s -> s { currentAlgo = Atkinson })
            Just "Ordered"           -> atomically $ modifyTVar' appState (\s -> s { currentAlgo = Ordered })
            Just "SimpleQuantization"-> atomically $ modifyTVar' appState (\s -> s { currentAlgo = SimpleQuantization })
            _                        -> return ()
    
    void $ on metricCombo #changed $ do
        mText <- Gtk.comboBoxTextGetActiveText metricCombo
        case mText of
            Just "Euclidean" -> atomically $ modifyTVar' appState (\s -> s { currentMetric = Euclidean })
            Just "Manhattan" -> atomically $ modifyTVar' appState (\s -> s { currentMetric = Manhattan })
            _                -> return ()

    void $ on bitsScale #valueChanged $ do
        val <- round <$> #getValue bitsScale
        atomically $ modifyTVar' appState (\s -> s { currentNBits = val })

    -- Save button logic is unchanged and now works correctly, as it saves the
    -- dithered image stored in `displayedImage`.
    void $ on saveBtn #clicked $ do
        mImg <- readTVarIO displayedImage
        when (isJust mImg) $ do
            dialog <- new Gtk.FileChooserDialog [ #title := "Save File", #action := Gtk.FileChooserActionSave ]
            Gtk.dialogAddButton dialog "Cancel" (fromIntegral $ fromEnum Gtk.ResponseTypeCancel)
            Gtk.dialogAddButton dialog "Save"   (fromIntegral $ fromEnum Gtk.ResponseTypeAccept)
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