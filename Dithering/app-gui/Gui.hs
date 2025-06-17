-- app-gui/Gui.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLabels #-}

import qualified GI.Gtk as Gtk
import qualified GI.Gdk as Gdk
import qualified GI.GdkPixbuf as GdkPixbuf
import qualified GI.GLib as GLib
import Data.GI.Base
import Control.Monad (void, when)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.STM
import qualified Data.Vector.Storable.Mutable as M
import Control.Monad.ST (runST)
import Data.Int (Int32)
import Data.Maybe (isJust, fromJust)
import qualified Data.Vector.Storable as V
import qualified Data.ByteString as BS

import Dithering.Core (DitheringAlgorithm(..), PaletteMetric(..), ImageRGB, applyDithering, vectorToImage, imageToDoubleVector, getImageDimensions, ditherVectorInPlace, atkinsonVectorInPlace, paletteFuncFor)
import Codec.Picture (readImage, writePng, convertRGB8, imageWidth, imageHeight, imageData)

imageRGBToPixbuf :: ImageRGB -> IO GdkPixbuf.Pixbuf
imageRGBToPixbuf img = do
    let vec = imageData img
        byteString = BS.pack $ V.toList vec
    bytes <- GLib.bytesNew (Just byteString)
    GdkPixbuf.pixbufNewFromBytes bytes GdkPixbuf.ColorspaceRgb False 8
        (fromIntegral $ imageWidth img)
        (fromIntegral $ imageHeight img)
        (fromIntegral $ imageWidth img * 3)

data AppState = AppState
    { baseImage      :: Maybe ImageRGB
    , displayedImage :: Maybe ImageRGB
    , currentAlgo    :: DitheringAlgorithm
    , currentMetric  :: PaletteMetric
    , currentNBits   :: Int
    , zoomLevel      :: Double
    , fitToWindow    :: Bool
    }

initialState :: AppState
initialState = AppState
    { baseImage      = Nothing
    , displayedImage = Nothing
    , currentAlgo    = FloydSteinberg
    , currentMetric  = Euclidean
    , currentNBits   = 3
    , zoomLevel      = 1.0
    , fitToWindow    = False
    }

data UI = UI
    { uiWin          :: Gtk.ApplicationWindow
    , uiScrolledWin  :: Gtk.ScrolledWindow
    , uiImgDisplay   :: Gtk.Image
    , uiRunBtn       :: Gtk.Button
    , uiZoomScale    :: Gtk.Scale
    , uiZoomFitCheck :: Gtk.CheckButton
    , uiZoomResetBtn :: Gtk.Button
    , uiAppState     :: TVar AppState
    , uiBasePixbuf   :: TVar (Maybe GdkPixbuf.Pixbuf)
    }

main :: IO ()
main = do
    app <- new Gtk.Application [#applicationId := "com.github.tomasz-niedzialek.dithering"]
    void $ on app #activate (onActivate app)
    void $ #run app Nothing

onActivate :: Gtk.Application -> IO ()
onActivate app = do
    appStateTVar <- newTVarIO initialState
    basePixbufTVar <- newTVarIO Nothing
    win' <- new Gtk.ApplicationWindow [#application := app, #title := "Haskell Dithering", #defaultWidth := 800, #defaultHeight := 600]
    grid <- new Gtk.Grid [#columnSpacing := 10, #rowSpacing := 10, #margin := 10]
    scrolledWin' <- new Gtk.ScrolledWindow []; #setHexpand scrolledWin' True; #setVexpand scrolledWin' True
    #setShadowType scrolledWin' Gtk.ShadowTypeIn
    imgDisplay' <- new Gtk.Image []; #add scrolledWin' imgDisplay'
    Gtk.gridAttach grid scrolledWin' 0 0 1 10
    controlsBox <- new Gtk.Box [#orientation := Gtk.OrientationVertical, #spacing := 15]; #setHexpand controlsBox False
    Gtk.gridAttach grid controlsBox 1 0 1 1
    fileBtn <- new Gtk.FileChooserButton [#title := "Choose an Image", #action := Gtk.FileChooserActionOpen]; #packStart controlsBox fileBtn False False 5
    algoLabel <- new Gtk.Label [#label := "Algorithm"]; #packStart controlsBox algoLabel False False 0
    algoCombo <- new Gtk.ComboBoxText []; Gtk.comboBoxTextAppendText algoCombo "FloydSteinberg"; Gtk.comboBoxTextAppendText algoCombo "Atkinson"; Gtk.comboBoxTextAppendText algoCombo "Ordered"; Gtk.comboBoxTextAppendText algoCombo "SimpleQuantization"; #setActive algoCombo 0; #packStart controlsBox algoCombo False False 5
    metricLabel <- new Gtk.Label [#label := "Palette Metric"]; #packStart controlsBox metricLabel False False 0
    metricCombo <- new Gtk.ComboBoxText []; Gtk.comboBoxTextAppendText metricCombo "Euclidean"; Gtk.comboBoxTextAppendText metricCombo "Manhattan"; #setActive metricCombo 0; #packStart controlsBox metricCombo False False 5
    bitsLabel <- new Gtk.Label [#label := "Bits per channel (1-8)"]; #packStart controlsBox bitsLabel False False 0
    bitsScale <- new Gtk.Scale [#orientation := Gtk.OrientationHorizontal, #drawValue := True]; #setRange bitsScale 1 8; #setValue bitsScale 3; #setDigits bitsScale 0; #packStart controlsBox bitsScale False False 5
    runBtn' <- new Gtk.Button [#label := "Dither!", #sensitive := False]; #packStart controlsBox runBtn' False False 10
    zoomLabel <- new Gtk.Label [#label := "Zoom"]; #packStart controlsBox zoomLabel False False 0
    zoomScale' <- new Gtk.Scale [#orientation := Gtk.OrientationHorizontal, #drawValue := True, #sensitive := False]; #setRange zoomScale' 10 200; #setValue zoomScale' 100; #setDigits zoomScale' 0; #packStart controlsBox zoomScale' False False 5
    zoomFitCheck' <- new Gtk.CheckButton [#label := "Fit to Window", #sensitive := False]; #packStart controlsBox zoomFitCheck' False False 5
    zoomResetBtn' <- new Gtk.Button [#label := "Reset Zoom (100%)", #sensitive := False]; #packStart controlsBox zoomResetBtn' False False 5
    saveBtn <- new Gtk.Button [#label := "Save Dithered Image..."]; #packStart controlsBox saveBtn False False 10
    #add win' grid
    let ui = UI { uiWin = win', uiScrolledWin = scrolledWin', uiImgDisplay = imgDisplay', uiRunBtn = runBtn', uiZoomScale = zoomScale', uiZoomFitCheck = zoomFitCheck', uiZoomResetBtn = zoomResetBtn', uiAppState = appStateTVar, uiBasePixbuf = basePixbufTVar }
    void $ on fileBtn #selectionChanged (#getFilename fileBtn >>= maybe (return ()) (loadAndDisplayOriginalImage ui))
    void $ on (uiRunBtn ui) #clicked (runDithering ui)
    void $ on algoCombo #changed (Gtk.comboBoxTextGetActiveText algoCombo >>= maybe (return ()) (\t -> atomically $ modifyTVar' (uiAppState ui) $ \s -> s { currentAlgo = case t of "Atkinson" -> Atkinson; "Ordered" -> Ordered; "SimpleQuantization" -> SimpleQuantization; _ -> FloydSteinberg } ))
    void $ on metricCombo #changed (Gtk.comboBoxTextGetActiveText metricCombo >>= maybe (return ()) (\t -> atomically $ modifyTVar' (uiAppState ui) $ \s -> s { currentMetric = if t == "Manhattan" then Manhattan else Euclidean }))
    void $ on bitsScale #valueChanged (round <$> #getValue bitsScale >>= \val -> atomically $ modifyTVar' (uiAppState ui) $ \s -> s { currentNBits = val })
    void $ on (uiZoomScale ui) #valueChanged (do val <- #getValue (uiZoomScale ui); atomically $ modifyTVar' (uiAppState ui) $ \s -> s { zoomLevel = val / 100.0 }; updateDisplayedImage ui)
    void $ on (uiZoomFitCheck ui) #toggled (do active <- #getActive (uiZoomFitCheck ui); atomically $ modifyTVar' (uiAppState ui) $ \s -> s { fitToWindow = active }; #setSensitive (uiZoomScale ui) (not active); updateDisplayedImage ui)
    void $ on (uiZoomResetBtn ui) #clicked (do atomically $ modifyTVar' (uiAppState ui) $ \s -> s { zoomLevel = 1.0, fitToWindow = False }; #setValue (uiZoomScale ui) 100; #setActive (uiZoomFitCheck ui) False; updateDisplayedImage ui)
    void $ on (uiScrolledWin ui) #configureEvent (\_ -> do state <- readTVarIO (uiAppState ui); when (fitToWindow state) (updateDisplayedImage ui); return False)
    void $ on saveBtn #clicked (saveDitheredImage ui)
    #showAll win'

updateDisplayedImage :: UI -> IO ()
updateDisplayedImage ui = void . forkIO $ readTVarIO (uiAppState ui) >>= \state -> readTVarIO (uiBasePixbuf ui) >>= maybe (return ()) (go state)
  where go state pbuf = do
          (pWidth, pHeight) <- (,) <$> GdkPixbuf.pixbufGetWidth pbuf <*> GdkPixbuf.pixbufGetHeight pbuf
          (cWidth, cHeight) <- (,) <$> #getAllocatedWidth (uiScrolledWin ui) <*> #getAllocatedHeight (uiScrolledWin ui)
          let (newWidth, newHeight) = if fitToWindow state
                then let scale = min (fromIntegral cWidth / fromIntegral pWidth) (fromIntegral cHeight / fromIntegral pHeight) in (floor $ fromIntegral pWidth * scale, floor $ fromIntegral pHeight * scale)
                else (floor $ fromIntegral pWidth * zoomLevel state, floor $ fromIntegral pHeight * zoomLevel state)
          mScaled <- GdkPixbuf.pixbufScaleSimple pbuf (fromIntegral newWidth) (fromIntegral newHeight) GdkPixbuf.InterpTypeBilinear
          void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ #setFromPixbuf (uiImgDisplay ui) mScaled >> return False

runDithering :: UI -> IO ()
runDithering ui = do
  state <- readTVarIO (uiAppState ui)
  case baseImage state of
    Nothing -> return ()
    Just img -> void . forkIO $ do
      void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ #setSensitive (uiRunBtn ui) False >> return False
      
      let (w, h) = getImageDimensions img
      let rawD = imageToDoubleVector img
      let paletteFunc = paletteFuncFor (currentMetric state)
      
      putStrLn "Stage 1: Quantizing..."
      let quantD = applyDithering SimpleQuantization (currentMetric state) (currentNBits state) w h rawD
      let quantImg = vectorToImage w h quantD
      quantPixbuf <- imageRGBToPixbuf quantImg
      
      atomically $ do
        modifyTVar' (uiAppState ui) (\s -> s { displayedImage = Just quantImg })
        writeTVar (uiBasePixbuf ui) (Just quantPixbuf)
      updateDisplayedImage ui
      
      when (currentAlgo state /= SimpleQuantization) $ do
        threadDelay 1000000
        putStrLn "Stage 2: Dithering..."
        let ditheredD = runST $ do
              mv <- M.new (V.length rawD)
              case currentAlgo state of
                FloydSteinberg -> ditherVectorInPlace paletteFunc (currentNBits state) w h rawD mv
                Atkinson       -> atkinsonVectorInPlace paletteFunc (currentNBits state) w h rawD mv
                Ordered        -> V.copy mv (applyDithering Ordered (currentMetric state) (currentNBits state) w h rawD)
                _              -> V.copy mv quantD
              V.freeze mv

        -- **THE FIX IS HERE:** The let and monadic bindings are on separate lines.
        let ditheredImg = vectorToImage w h ditheredD
        ditheredPixbuf <- imageRGBToPixbuf ditheredImg
        
        atomically $ do
          modifyTVar' (uiAppState ui) (\s -> s { displayedImage = Just ditheredImg })
          writeTVar (uiBasePixbuf ui) (Just ditheredPixbuf)
        updateDisplayedImage ui
        
      putStrLn "All processing finished."
      void $ GLib.idleAdd GLib.PRIORITY_DEFAULT_IDLE $ #setSensitive (uiRunBtn ui) True >> return False

loadAndDisplayOriginalImage :: UI -> FilePath -> IO ()
loadAndDisplayOriginalImage ui path = do
  eImg <- liftIO $ readImage path
  case eImg of
    Left err -> putStrLn $ "Error loading image: " ++ err
    Right dImg -> do
        let img = convertRGB8 dImg
        originalPixbuf <- imageRGBToPixbuf img
        atomically $ do
            modifyTVar' (uiAppState ui) (\s -> s { baseImage = Just img, displayedImage = Just img })
            writeTVar (uiBasePixbuf ui) (Just originalPixbuf)
        #setSensitive (uiRunBtn ui) True
        #setSensitive (uiZoomScale ui) True
        #setSensitive (uiZoomFitCheck ui) True
        #setSensitive (uiZoomResetBtn ui) True
        updateDisplayedImage ui

saveDitheredImage :: UI -> IO ()
saveDitheredImage ui = do
    mImg <- displayedImage <$> readTVarIO (uiAppState ui)
    when (isJust mImg) $ do
        dialog <- new Gtk.FileChooserDialog [ #title := "Save File", #action := Gtk.FileChooserActionSave ]
        Gtk.dialogAddButton dialog "Cancel" (fromIntegral $ fromEnum Gtk.ResponseTypeCancel)
        Gtk.dialogAddButton dialog "Save"   (fromIntegral $ fromEnum Gtk.ResponseTypeAccept)
        #setTransientFor dialog (Just (uiWin ui))
        response <- #run dialog
        when (toEnum (fromIntegral response) == Gtk.ResponseTypeAccept) $ do
            mFile <- #getFilename dialog
            case mFile of Nothing -> return (); Just path -> liftIO $ writePng path (fromJust mImg)
        #destroy dialog