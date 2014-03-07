{-# LANGUAGE TemplateHaskell, OverloadedStrings, LambdaCase#-}

module Main (main) where

import Text.Printf
import Prelude hiding (sum)
import Foreign.C 
import Control.Monad (when, forever, join)
import Control.Concurrent (threadDelay)
import Data.Word
import Foreign (Ptr, nullPtr, allocaArray, peekElemOff, pokeElemOff, sizeOf)
import Foreign.Marshal.Alloc (mallocBytes) 
import Foreign.Marshal.Utils (copyBytes, with) 
import System.Exit (exitFailure, exitSuccess)
import System.IO 
import Control.Concurrent
import Control.Concurrent.STM
import qualified Sound.JACK as Jack
import Sound.JACK.Audio as JackA
import Graphics.UI.GLUT hiding (PixelFormat, histogram, imageHeight)
import Graphics.V4L2
import Control.Applicative
import Data.Array.CArray
import Data.Array.IOCArray
import Control.Monad.Trans
import qualified Control.Monad.Trans.Class as Trans
import Unsafe.Coerce
import Foreign.C.Types
import System.Random (randomIO)
import Data.Complex
import Control.Arrow ((***))
import Data.Array.IO
import CounterBalance
--import Data.WAVE
import System.Process (system)
import System.Posix
import System.Directory

foreign import ccall "setup" jackSetup :: CInt -> IO ()
foreign import ccall "setDelay" jackSetDelay :: CFloat -> IO ()
foreign import ccall "jackClose" jackClose :: IO ()

delayDur = 0.2

main = do
  (progname, args) <- getArgsAndInitialize
  subjectID <- case args of
    [] -> do
      cursubs <- getDirectoryContents "data" 
      print cursubs
      return $ 1 + maximum [read $ takeWhile (/= '.') f|f <- cursubs, length f > 3]
    subjectID:args -> return $ read subjectID
  let video = parseArgs args 
  exists <- doesFileExist $ "data/" ++ show subjectID ++ ".wav"
  if exists then error "Subject exists, will not overwrite" else return ()
  es <- experiment
  let run = es !! subjectID
  (vin, vout) <- createPipe
  vhand <- fdToHandle vout
  saveVideo vin vout ("./data/" ++ show subjectID ++ ".mp4")
  closeFd vin
  jackSetup (toEnum subjectID)
  jackSetDelay 0.0
  glut run vhand video

parseArgs ["-v",i]= case i of 
  "1" -> "/dev/video1"
  "0" -> "/dev/video0"
parseArgs _ = "/dev/video0" 

glut run vhand video = do
  d <- openDevice video
  f <- setFormat d Capture . 
    (\f->f{ imagePixelFormat = PixelRGB24 }) =<< getFormat d Capture
  initialDisplayMode $= [ RGBMode, DoubleBuffered, WithDepthBuffer ]
  --windowHint WindowHint'DepthBits 16
  createWindow "Delayed Audio Feedback"
  texture Texture2D $= Enabled
  [ti] <- genObjectNames 1
  textureBinding Texture2D $= Just ti
  texImage2D 
    Texture2D 
    NoProxy 
    0 
    RGBA' 
    (TextureSize2D (fromIntegral (imageWidth f)) (fromIntegral (imageHeight f))) 
    0 
    (PixelData RGBA UnsignedByte nullPtr)
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
  frame <- atomically $ newTVar (nullPtr)
  video <- atomically $ newTVar (Just Video)
  picture <- mallocBytes $ imageWidth f * imageHeight f * 3
  ind <- atomically $ newTVar run
  idleCallback $= Just (idle d f vhand frame)
  displayCallback $= display f ti video frame picture
  reshapeCallback $= Just (resize f)
  depthFunc $= Just Less
  keyboardMouseCallback $= Just (\k s _ _ -> case (k,s) of
    (Char 'p', Down) -> withFrame d f $ \p n -> do
      copyBytes picture p (imageWidth f * imageHeight f * 3) 
      atomically $ writeTVar video (Just NoVideo)
      forkIO $ threadDelay 3000000 >> atomically (writeTVar video (Just Video))
      return ()
    (Char 'q', Down) -> do
      jackClose
      exitSuccess
    (Char ' ', Down) -> do
      forkIO (threadDelay 1000000 >> nextStim ind video)
      return ()
    _ -> return ())
  mainLoop

nextStim ind video = atomically (readTVar ind) >>= \case
  [] -> jackClose >> exitSuccess
  []:bs -> do atomically $ writeTVar ind bs 
              atomically $ writeTVar video Nothing
  ((s,(v,a)):stims):bs -> do
    putStrLn $ "Running stimulus: " ++ s ++ ": " ++ show (v, a)
    let filename = printf "stimuli/%s.wav" s
    putStrLn $ "playing " ++ filename
    forkOS $ do x <- system $ "mplayer -af extrastereo=0 -ao jack " ++ filename ++ " 2>> mplayer.log"; return () 
    atomically $ writeTVar video (Just v)
    atomically $ writeTVar ind $ stims:bs
    case a of
      Delay -> jackSetDelay delayDur
      NoDelay -> jackSetDelay 0.0

resize f (Size w h) = do
  viewport $= (Position 0 0, Size w h)
  matrixMode $= Projection
  loadIdentity
  let hs = fromIntegral h
      ws = fromIntegral w
      hi = fromIntegral $ imageHeight f
      wi = fromIntegral $ imageWidth f
  let aspect = (hi * ws) / (wi * hs)
  ortho (-aspect) aspect (-1) 1 (-1) 1 
  return ()

idle d f phand frame = withFrame d f $ \p n -> do
  atomically $ writeTVar frame p
  let (w,h) = (imageWidth f, imageHeight f)
  hPutStr phand $ "P6\n" ++ show w ++ " " ++ show h ++ " 255\n"
  hPutBuf phand p (w * h * 3)

display f ti vid frame pic = do
  clear [ColorBuffer, DepthBuffer]
  
  -- Display image (Video, Picture, or Blank)
  b <- atomically $ readTVar vid 
  let (w,h) = (imageWidth f, imageHeight f)
  p <- atomically $ readTVar frame
  case b of 
    Nothing -> do
      blank <- mallocBytes (w * h * 3)
      setTexture blank w h
    Just Video -> setTexture p w h
    Just NoVideo -> setTexture pic w h
  renderPrimitive Quads $ do
    corner 0 0
    corner 0 1
    corner 1 1
    corner 1 0
  
  --Display text
  case b of
    Nothing -> do
      color (Color3 1.0 1.0 1.0 :: Color3 GLfloat)
      rasterPos (Vertex2 (0.0) (0.0) :: Vertex2 GLfloat) 
      --renderString Helvetica18 "Testing ..."
    _ -> return ()

  swapBuffers
  postRedisplay Nothing

corner :: GLfloat -> GLfloat -> IO ()
corner x y = texCoord (TexCoord2 x y) >> vertex (Vertex2 (1 - (2*x)) (1 - (2*y)))

setTexture buf w h = texSubImage2D
  Texture2D
  0
  (TexturePosition2D 0 0)
  (TextureSize2D (fromIntegral w) (fromIntegral h))
  (PixelData RGB UnsignedByte buf)

-- ffmpeg pipe for saving video
saveVideo :: Fd -> Fd -> String -> IO CPid
saveVideo pin pout filename = forkProcess $ do
  closeFd pout
  dupTo pin stdInput
  system $ "ffmpeg -r 15 -f image2pipe -vcodec ppm -i - " ++ filename ++ " 2>ffmpeg.log"
  return ()
