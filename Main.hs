{-# LANGUAGE TemplateHaskell, OverloadedStrings, LambdaCase#-}

module Main (main) where

import Prelude hiding (sum)

import Control.Monad (when, forever, join)
import Control.Concurrent (threadDelay)
import Data.Word (Word8, Word32)
import Foreign (Ptr, nullPtr, allocaArray, peekElemOff, pokeElemOff)
import Foreign.Marshal.Alloc (mallocBytes) 
import Foreign.Marshal.Utils (copyBytes) 
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
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
import Data.WAVE

-- Delay
delayDur = 0.2

-- Cooley-Tukey
ifft xs = map (/(fromIntegral $ length xs)) . fft . map conjugate $ xs

fft [] = []
fft [x] = [x]
fft xs = zipWith (+) ys ts ++ zipWith (-) ys ts
    where (ys, zs) = join (***) fft $ unzip $ split xs
          split xs = case xs of (x:y:xs) -> (x,y):split xs; _ -> []
          ts = zipWith (\z k -> exp' k n  * z) zs [0..]
          exp' k n = cis $ -2.0 * pi * (fromIntegral k) / (fromIntegral n)
          n = (length xs)

lowPassFilter freq db fs = zipWith filter fs [0..]
  where filter v f = if (f/n)*sampleRate > freq then v/2**(db/3) else v*2**(db/3)
        n = fromIntegral $ length fs 

window xs = zipWith (*) [exp (-(0.5 * ((i - (n/2-0.5)) / (n/4 * 0.5))**2)) | i <- [0..]] xs
  where n = fromIntegral $ length xs

type RingBuffer a = (IOUArray Int a, Int)

sampleRate = 48000

main = do
  (progname, subjectID:args) <- getArgsAndInitialize
  es <- experiment
  let run = es !! (read subjectID :: Int)
  rba <- atomically . newTVar =<< flip (,) 0 <$> newArray (0,16) 0.0 
  acb <- atomically $ newTVar (next rba)
  forkIO $ jack progname (withCB acb)
  glut run acb

jack name cb = do
  Jack.handleExceptions $ do
    client <- Jack.newClientDefault name 
    input <- Jack.newPort client "input"
    output <- Jack.newPort client "output"
    JackA.withProcessMono client input cb output $ do
      Jack.activate client
      Jack.connect client ("system:capture_1") (name ++ ":input")
      Jack.connect client (name ++ ":output") ("system:playback_1")
      Jack.connect client (name ++ ":output") ("system:playback_2")
      Trans.lift Jack.waitForBreak

withCB :: TVar (Sample -> IO Sample) -> Sample -> IO Sample
withCB cb s = atomically (readTVar cb) >>= \f->f s

next :: TVar (RingBuffer Double) -> Sample -> IO Sample
next rb s = do
  (a, i) <- atomically $ readTVar rb
  bound <- snd <$> getBounds a
  v' <- readArray a i
  writeArray a i (realToFrac s)
  atomically $ modifyTVar rb $ (\(b,i) -> (b, (i+1) `mod` bound))
  noise <- (/25) <$> randomIO :: IO Double
  return $ realToFrac $ (v' + noise)

glut run cb = do
  d <- openDevice "/dev/video0"
  f <- setFormat d Capture . 
    (\f->f{ imagePixelFormat = PixelRGB24 }) =<< getFormat d Capture
  createWindow "Delayed Audio Feedback"
  texture Texture2D $= Enabled
  [ti] <- genObjectNames 1
  textureBinding Texture2D $= Just ti
  texImage2D Texture2D NoProxy 0 RGBA' 
    (TextureSize2D (fromIntegral (imageWidth f)) (fromIntegral (imageHeight f))) 
    0 (PixelData RGBA UnsignedByte nullPtr)
  textureFilter Texture2D $= ((Nearest, Nothing), Nearest)
  textureWrapMode Texture2D T $= (Repeated, ClampToEdge)
  video <- atomically $ newTVar Video
  picture <- mallocBytes $ imageWidth f * imageHeight f * 3
  ind <- atomically $ newTVar (0,0)
  idleCallback $= Just (idle d f video picture)
  displayCallback $= display f ti
  reshapeCallback $= Just (resize f)
  keyboardMouseCallback $= Just (\k s _ _ -> case (k,s) of
    (Char 'p', Down) -> withFrame d f $ \p n -> do
      copyBytes picture p (imageWidth f * imageHeight f * 3) 
      atomically $ writeTVar video Video
      forkIO $ threadDelay 1000000 >> atomically (writeTVar video Video)
      return ()
    (Char 'q', Down) -> do
      exitSuccess
    (Char ' ', Down) -> nextStim ind run video cb
    _ -> return ())
  mainLoop

delay :: Float -> TVar (Sample -> IO (Sample)) -> IO () 
delay dur mv = do
  rba <- flip (,) 0 <$> newArray (0, max 1 $ round $ dur * sampleRate) 0.0
  atomically . writeTVar mv . next =<< atomically (newTVar rba) 

nextStim ind run video cb = atomically (readTVar ind) >>= \case
  (b,i) | i >= 8 -> do atomically $ writeTVar ind (b+1,0)
                       nextStim ind run video cb
  (b,i) | b >= 8 -> exitSuccess
  (b,i) -> let (s,(v,a)) = (run !! b) !! i in do
    putStrLn $ "Running stimulus: " ++ show s ++ ": " ++ show (v, a)
    playWave <$> loadWave (show s ++ ".wav") <*> return cb <*> atomically (readTVar cb)
    atomically $ writeTVar video v
    atomically $ writeTVar ind (b,i+1)
    case a of
      Delay -> delay delayDur cb
      NoDelay -> delay 0 cb

resize f (Size w h) = do
  viewport $= (Position 0 0, Size w h)
  matrixMode $= Projection
  loadIdentity
  let aspect = (fromIntegral $ imageHeight f * fromIntegral w) / (fromIntegral $ imageWidth f * fromIntegral h)
  ortho (-aspect) aspect (-1) 1 (-1) 1 

idle d f vid pic = withFrame d f $ \p n -> do
    b <- atomically $ readTVar vid 
    case b of 
      Video -> setVideo p
      NoVideo -> setVideo pic
    postRedisplay Nothing      
  where setVideo buf = texSubImage2D Texture2D 0 (TexturePosition2D 0 0) (TextureSize2D (fromIntegral $ imageWidth f) (fromIntegral $ imageHeight f)) (PixelData RGB UnsignedByte buf)

display f ti = do
  clear [DepthBuffer, ColorBuffer]
  renderPrimitive Quads (u 0 0 >> u 0 1 >> u 1 1 >> u 1 0)
  swapBuffers
  where
    u :: GLfloat -> GLfloat -> IO ()
    u x y = texCoord (TexCoord2 x y) >> vertex (Vertex2 (1 - (2*x)) (1 - (2*y)))

-- Wave utilites
saveWave :: String -> [Double] -> IO ()
saveWave f d = putWAVEFile f (WAVE header dat)   
  where header = WAVEHeader 1 (48000 `div` frameSize) 32 Nothing 
        dat = let split = \case [] -> []; xs -> uncurry (:) . fmap split . splitAt frameSize $ xs in split (map doubleToSample d)
        frameSize = 16

loadWave :: String -> IO [Double]
loadWave f = map sampleToDouble . concat . waveSamples <$> getWAVEFile f

playWave :: [Double] -> TVar (Sample -> IO Sample) -> (Sample -> IO Sample) -> IO ()
playWave ds cb cont = do
  atomically $ writeTVar cb $ const $ pw ds
  where pw :: [Double] -> IO Sample 
        pw [] = do atomically $ writeTVar cb $ cont 
                   return 0
        pw (x:xs) = do atomically $ writeTVar cb $ const $ pw xs
                       return (realToFrac x)
