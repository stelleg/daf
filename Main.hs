{-# LANGUAGE TemplateHaskell, OverloadedStrings, LambdaCase#-}
{-# CFILES jack.c gltext.c #-}

module Main (main) where

import Text.Printf
import Prelude hiding (sum)
import Foreign.C 
import Control.Monad (when, forever, join)
import Control.Concurrent (threadDelay)
import Data.Word
import Foreign (Ptr, nullPtr, allocaArray, peekElemOff, pokeElemOff, sizeOf)
import Foreign.Marshal.Alloc (mallocBytes, free) 
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
import System.Posix.Signals (signalProcess, sigKILL)
import System.FilePath.Posix
import System.Directory

foreign import ccall "setup" jackSetup :: CInt -> IO ()
foreign import ccall "setDelay" jackSetDelay :: CFloat -> IO ()
foreign import ccall "jackClose" jackClose :: IO ()
foreign import ccall "print_at" printAt :: CFloat -> CFloat -> CFloat -> CWString -> IO ()

delayDur = 0.18

main = do
  (progname, args) <- getArgsAndInitialize
  let (video, maybeSubjectID) = parseArgs args
  subjectID <- case maybeSubjectID of
    Just n -> return n 
    Nothing -> do 
      cursubs <- getDirectoryContents "data" 
      return $ 1 + maximum [read $ dropExtension f|f <- cursubs ++ ["-1.wav"], 
                                                 takeExtension f == ".wav"]
  print (video, subjectID)
  exists <- doesFileExist $ "data/" ++ show subjectID ++ ".wav"
  if exists 
    then error "Subject exists, will not overwrite" 
    else printf "Running subject %d\n" subjectID
  es <- experiment
  let run = es !! subjectID
  (vin, vout) <- createPipe
  vhand <- fdToHandle vout
  saveVideo vin vout ("./data/" ++ show subjectID ++ ".mp4")
  closeFd vin
  jackSetup (toEnum subjectID)
  jackSetDelay 0.0
  glut run vhand video

type VideoDevice = String
type SubjectID = Int

parseArgs :: [String] -> (VideoDevice, Maybe SubjectID)
parseArgs args = parseArgs' args ("/dev/video0", Nothing)
parseArgs' ("-n":i:args) (v, sid) = case read i of
  n -> parseArgs' args (v, Just n)
parseArgs' ("-v":i:args) (v, sid) = parseArgs' args (i, sid)
parseArgs' [] (v, sid) = (v, sid)

glut run vhand video = do
  d <- openDevice video
  f <- setFormat d Capture . 
    (\f->f{ 
      imagePixelFormat = PixelRGB24,
      imageWidth = 1280,
      imageHeight = 720 }) =<< getFormat d Capture
  initialDisplayMode $= [ RGBMode, DoubleBuffered, WithDepthBuffer ]
  createWindow "Delayed Audio Feedback"
  let (w,h) = (imageWidth f, imageHeight f)
  frame <- atomically $ newTVar (nullPtr)
  video <- atomically $ newTVar V
  picture <- mallocBytes $ w * h * 3
  ind <- atomically $ newTVar run
  [ti] <- genObjectNames 1
  idleCallback $= Just (idle d f vhand frame)
  displayCallback $= display f video frame picture ti
  reshapeCallback $= Just (resize f)
  depthFunc $= Just Less
  keyboardMouseCallback $= Just (\k s _ _ -> case (k,s) of
    (Char 'p', Down) -> withFrame d f $ \p n -> do
      copyBytes picture p (imageWidth f * imageHeight f * 3) 
      atomically $ writeTVar video P 
      forkIO $ threadDelay 3000000 >> atomically (writeTVar video V)
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
  [] -> do
    atomically $ writeTVar video (Instruction "Done, thank you")
    jackClose
    threadDelay 3000000
    exitSuccess
  []:[] -> do
    atomically $ writeTVar video (Instruction "Done, thank you")
    jackClose
    threadDelay 3000000
    exitSuccess
  []:bs -> do 
    v <- atomically $ readTVar video
    case v of 
      Instruction "Take a break! Press the spacebar when you are ready to continue." -> do
        atomically $ writeTVar video $ Instruction "Don't forget to look at the screen! Press spacebar when ready."
        atomically $ writeTVar ind bs
      _ -> do
        atomically $ writeTVar video $ Instruction "Take a break! Press the spacebar when you are ready to continue."
        
  ((s,(a,v)):stims):bs -> do
    putStrLn $ "Running stimulus: " ++ s ++ ": " ++ show (a, v)
    let filename = printf "stimuli/%s.wav" s
    putStrLn $ "playing " ++ filename
    forkOS $ do x <- system $ "mplayer -af extrastereo=0 -ao jack " ++ filename ++ " 2>> logs/mplayer.log"; return () 
    atomically $ writeTVar video v
    atomically $ writeTVar ind $ stims:bs
    case a of
      DAF -> jackSetDelay delayDur
      NAF -> jackSetDelay 0.0

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
  postRedisplay Nothing

idle d f phand frame = withFrame d f $ \p n -> do
  atomically $ writeTVar frame p
  let (w,h) = (imageWidth f, imageHeight f)
  hPutStr phand $ "P6\n" ++ show w ++ " " ++ show h ++ " 255\n"
  hPutBuf phand p (w * h * 3)
  postRedisplay Nothing

drawQuads :: [(GLfloat, GLfloat)] -> IO ()
drawQuads = renderPrimitive Quads . mapM_ (vertex . uncurry Vertex2)

display f vid frame pic ti = do
  clear [ColorBuffer, DepthBuffer]
  b <- atomically $ readTVar vid 
  let (w,h) = (imageWidth f, imageHeight f)
  p <- atomically $ readTVar frame
  case b of 
    Instruction s -> do
      color (Color3 1 1 1 :: Color3 GLfloat)
      s' <- newCWString s 
      printAt 4.0 (-4.0 * fromIntegral (length s)) (0) s'
      free s'
    N -> do
      color (Color3 1 1 1 :: Color3 GLfloat)
      s' <- newCWString "x"
      printAt 10.0 (-4.0) 0 s'
      free s'
    a -> do
      case a of 
        V -> setTexture p w h ti
        P -> setTexture pic w h ti
      renderPrimitive Quads $ do
        corner 0 0
        corner 0 1
        corner 1 1
        corner 1 0
  swapBuffers

corner :: GLfloat -> GLfloat -> IO ()
corner x y = texCoord (TexCoord2 x y) >> vertex (Vertex2 (1-2*x) (1-2*y))

setTexture buf w h ti = do 
  texture Texture2D $= Enabled
  textureBinding Texture2D $= Just ti
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
  texImage2D 
    Texture2D 
    NoProxy 
    0 
    RGBA' 
    (TextureSize2D (fromIntegral w) (fromIntegral h)) 
    0 
    (PixelData RGBA UnsignedByte nullPtr)
  texSubImage2D
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
  system $ "ffmpeg -r 15 -f image2pipe -vcodec ppm -i - " ++ filename ++ " 2>logs/ffmpeg.log"
  return ()
