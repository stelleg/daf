{-# LANGUAGE LambdaCase #-}
{- This is Beth's counterbalancing design
 - High level description: 
 -  6 blocks per subject
 -  blocks determined by delay 
 -  blocks have equal number of video/novideo, but "randomized"
 -  2 groups
 -  group 1 starts with delay block
 -  group 2 starts with nodelay
 -}
module CounterBalance where
import Prelude hiding (lookup)
import Data.Function
import Data.List hiding (union,lookup)
import System.Random.Shuffle
import Control.Monad.Random
import Text.Printf
import Data.Map

data VisualCondition = V | P | N | Instruction String deriving (Ord, Eq)
data AudioCondition = DAF | NAF deriving (Enum, Bounded,  Ord, Eq)

instance Show VisualCondition where
  show (Instruction s) = s
  show V = "v"
  show P = "p"
  show N = ""

instance Show AudioCondition where
  show DAF = "daf"
  show NAF = "naf"

type Block = [(Stimulus,Condition)]
type Run = [Block]
type Condition = (AudioCondition, VisualCondition) 
type Stimulus = String
type Experiment = [Run]

-- TODO: map stimuli to ints in straightforward way.
subjects :: [[(Condition, Int)]]
subjects = cycle [
  [((NAF,N),1),((DAF,N),4),((DAF,P),5),((NAF,P),2),((NAF,V),3),((DAF,V),6)],
  [((NAF,P),3),((NAF,N),5),((NAF,V),1),((DAF,N),6),((DAF,V),2),((DAF,P),4)],
  [((NAF,V),2),((DAF,P),1),((NAF,N),3),((DAF,V),4),((NAF,P),6),((DAF,N),5)],
  [((DAF,N),5),((NAF,P),6),((DAF,V),4),((NAF,N),3),((DAF,P),1),((NAF,V),2)],
  [((DAF,P),4),((DAF,V),2),((DAF,N),6),((NAF,V),1),((NAF,N),5),((NAF,P),3)],
  [((DAF,V),6),((NAF,V),3),((NAF,P),2),((DAF,P),5),((DAF,N),4),((NAF,N),1)],
  [((DAF,V),3),((NAF,V),5),((NAF,P),1),((DAF,P),6),((DAF,N),2),((NAF,N),4)],
  [((DAF,P),2),((DAF,V),1),((DAF,N),3),((NAF,V),4),((NAF,N),6),((NAF,P),5)],
  [((DAF,N),1),((NAF,P),4),((DAF,V),5),((NAF,N),2),((DAF,P),3),((NAF,V),6)],
  [((NAF,V),6),((DAF,P),3),((NAF,N),2),((DAF,V),5),((NAF,P),4),((DAF,N),1)],
  [((NAF,P),5),((NAF,N),6),((NAF,V),4),((DAF,N),3),((DAF,V),1),((DAF,P),2)],
  [((NAF,N),4),((DAF,N),2),((DAF,P),6),((NAF,P),1),((NAF,V),5),((DAF,V),3)]]

groupedStimuli :: Map Int [Stimulus]
groupedStimuli = f 1 stimuli
  where f n [] = empty
        f n s = case splitAt 10 s of (s,ss) -> singleton n s `union` f (n+1) ss

unshuffledRuns :: [Run]
unshuffledRuns = fmap (fmap (\(c,i) -> case lookup i groupedStimuli of Just s -> [(s',c) | s' <- s])) subjects

pp :: Experiment -> String
pp es = concatMap ppsubject $ zip [0..] es
  where ppsubject (i, es) = concatMap (ppstim i) $ concat es
        ppstim i (stim, (v,a)) = printf "%d, %s, %s\n" (i::Int) stim (show a ++ show v)

stimuli :: [Stimulus]
stimuli = [printf "f%02d" i | i <- [1..nStim]]

nStim = 60 :: Int
blocksize = 10

seed = 394 -- IMPORTANT!!! Keep this constant accross subjects!!!

experiment :: IO Experiment
experiment = do
  runs <- setStdGen (mkStdGen seed) >> runs
  return $ fmap ([("practice" ++ show i, (NAF, Instruction "Practice")) | i <- [1..6]] :) runs

runs :: MonadRandom m => m [Run]
runs  = mapM (mapM shuffleM) $ take 128 unshuffledRuns

