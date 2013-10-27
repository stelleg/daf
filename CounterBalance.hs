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

data VisualCondition = Video | NoVideo deriving (Enum, Bounded, Show, Ord, Eq)
data AudioCondition = Delay | NoDelay deriving (Enum, Bounded, Show, Ord, Eq)

type Block = [(Stimulus,Condition)]
type Run = [Block]
type Condition = (VisualCondition, AudioCondition) 
type Stimulus = Int
type Experiment = [Run]

conditions :: [Condition]
conditions = cycle [(v,a) | 
  v <- [Video, NoVideo], 
  a <- [Delay, NoDelay]] 

stimuli :: [Stimulus]
stimuli = [1..nStim]

nStim = 64 :: Int
blocksize = 64 `div` 8

seed = 394 -- Keep this constant accross subjects!

experiment :: IO Experiment
experiment = setStdGen (mkStdGen seed) >> runs

runs :: MonadRandom m => m [Run]
runs  = return
      -- A hack to ensure we get [[Delay, Nodelay, ..], [Nodelay, Delay]]
      . zipWith (\r->if r`mod`2==0 then reverse else id) [0..] 
      . map block 
      =<< (mapM shuffleM
      . take 64
      . map (uncurry zip) $ zip (repeat stimuli) (iterate tail conditions))

block :: Block -> Run
block = concat . transpose . map split . groupBy (on (==) (snd.snd)) . sortBy (on compare (snd.snd))

split = \case [] -> []; xs -> uncurry (:) . fmap split . splitAt blocksize $ xs
