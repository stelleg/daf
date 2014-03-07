{-# LANGUAGE LambdaCase #-}
{- This is Beth's counterbalancing design
 - High level description: 
 -  8 blocks per subject
 -  blocks determined by delay 
 -  blocks have equal number of video/novideo, but "randomized"
 -  2 groups
 -  group 1 starts with delay block
 -  group 2 starts with nodelay
 -}
module CounterBalance where

import Data.Function
import Data.List
import System.Random.Shuffle
import Control.Monad.Random hiding (split)
import Text.Printf

data VisualCondition = Video | NoVideo deriving (Enum, Bounded,  Ord, Eq)
data AudioCondition = Delay | NoDelay deriving (Enum, Bounded,  Ord, Eq)

instance Show VisualCondition where
  show Video = "vf"
  show NoVideo = ""

instance Show AudioCondition where
  show Delay = "daf"
  show NoDelay = "naf"

type Block = [(Stimulus,Condition)]
type Run = [Block]
type Condition = (VisualCondition, AudioCondition) 
type Stimulus = String
type Experiment = [Run]

conditions :: [Condition]
conditions = cycle [(v,a) | 
  v <- [Video, NoVideo], 
  a <- [Delay, NoDelay]] 

pp :: Experiment -> String
pp es = concatMap ppsubject $ zip [0..] es
  where ppsubject (i, es) = concatMap (ppstim i) $ concat es
        ppstim i (stim, (v,a)) = printf "%d, %s, %s\n" (i::Int) stim (show a ++ show v)

stimuli :: [Stimulus]
stimuli = [printf "f%02d" i | i <- [1..nStim]]

nStim = 64 :: Int
blocksize = 64 `div` 8

seed = 394 -- IMPORTANT!!! Keep this constant accross subjects!!!

experiment :: IO Experiment
experiment = do
  runs <- setStdGen (mkStdGen seed) >> runs
  return $ map ([("practice" ++ show i, (NoVideo, NoDelay)) | i <- [1..6]] :) runs

runs :: MonadRandom m => m [Run]
runs  = return
      -- A hack to ensure we get [[Delay, Nodelay, ..], [Nodelay, Delay]]
      . zipWith (\r->if r`mod`2==0 then reverse else id) [0..] 
      =<< (mapM $ mapM shuffleM)
      . map block 
      =<< (mapM shuffleM
      . take 64
      . map (uncurry zip) $ zip (repeat stimuli) (iterate tail conditions))

block :: Block -> Run
block = concat . transpose . map split . groupBy (on (==) (snd.snd)) . sortBy (on compare (snd.snd))

normalizeVideo xs = (tvids ++ tnovids, nvids ++ nnovids) 
  where videos = [k | k@(s,(Video,ac)) <- xs]
        novideos = [k | k@(s,(NoVideo, ac)) <- xs]
        (tvids, nvids)= splitAt 4 videos
        (tnovids, nnovids) = splitAt 4 novideos

split = \case [] -> []; xs -> uncurry (:) . fmap split . normalizeVideo $ xs
