module HW6.T3
  ( Config (..)
  , Cell (..)
  , CellState (..)
  , Comonad19Grid
  
  , simulate
  , printGrid
  ) where

import Data.ListZipper (ListZipper (..))
import Data.Grid (Grid (..), gUp, gDown, gLeft, gRight, grToList)

import System.Random (StdGen, randomRs, mkStdGen)
import Control.Comonad (extract, extend)
import Control.Monad (forM_, liftM2)

-- |Parameters for evaluating new generation.
data Config = Config
  { probability :: Double   -- ^ 0 < p < 1, the probability of infection of a healthy person with an infected or illness person
  , incubationPeriod :: Int -- ^ time of (in days) incubation status
  , illnessDuration :: Int  -- ^ time of (in days) illness status
  , immunityDuration :: Int -- ^ time of (in days)
  } deriving Show

-- |State of human.
data CellState
  = Healthy
  | Infected Int
  | Ill Int
  | Immune Int
  deriving Show

-- |Cell (for human representations).
data Cell = Cell
  { cellState :: CellState
  , cellRand :: StdGen
  }

instance Show Cell where
  show Cell { cellState = Healthy    } = "_"
  show Cell { cellState = Infected _ } = "i"
  show Cell { cellState = Ill _      } = "#"
  show Cell { cellState = Immune _   } = "@"

type Comonad19Grid = Grid Cell

isInfected :: Cell -> Bool
isInfected Cell { cellState = (Infected n) }
 | n > 0     = True
isInfected Cell { cellState = (Ill n) }
 | n > 0     = True
isInfected _ = False

infectedNeighbours :: Grid Cell -> Int
infectedNeighbours g = infectedCount . flip map neighbours $ \direction ->
                     extract . direction $ g
  where
    infectedCount = length . filter isInfected
    neighbours    = horizontals ++ verticals ++ liftM2 (.) horizontals verticals
      where
        horizontals = [gLeft, gRight]
        verticals   = [gUp, gDown]

-- |Rules for evaluations new generation.
ruleImpl :: Config -- ^ parameters
         -> Int    -- ^ count of infected neighbours.
         -> Cell   -- ^ old
         -> Cell   -- ^ new (result).
ruleImpl c n Cell      { cellState = Immune 1  , cellRand = rand } = ruleImpl c n $ Cell Healthy rand
ruleImpl _ _ Cell      { cellState = Immune n  , cellRand = rand } = Cell (Immune $ n - 1) rand
ruleImpl c n Cell      { cellState = Infected 1, cellRand = rand } = ruleImpl c n $ Cell (Ill $ illnessDuration c) rand
ruleImpl _ _ Cell      { cellState = Infected n, cellRand = rand } = Cell (Infected $ n - 1) rand
ruleImpl c n Cell      { cellState = Ill 0     , cellRand = rand } = ruleImpl c n $ Cell (Immune $ immunityDuration c) rand
ruleImpl _ _ Cell      { cellState = Ill n     , cellRand = rand } = Cell (Ill $ n - 1) rand
ruleImpl c n cell@Cell { cellState = Healthy   , cellRand = rand } = if not (null reproduceInfected)
                                                                      then Cell (Infected $ incubationPeriod c) rand
                                                                      else cell
  where
    reproduceInfected = filter (<= probability c) $ take n $ randomRs (0, 1) rand

-- |Eval cell of new generation.
rule :: Config         -- ^ parameters
     -> Comonad19Grid  -- ^ current grid
     -> Cell           -- ^ cell of new generation.
rule config g = ruleImpl config (infectedNeighbours g) $ extract g

-- |Creates an infinite list of grids using the given configuration.
-- Each element of this list represents one infection simulation step.
simulate :: Config          -- ^ parameters
         -> Int             -- ^ seed for generate random generators
         -> [Comonad19Grid] -- ^ sequence of grids by generations (start contains one focused infected human only).
simulate config startSeed = iterate nextDay $ startGrid config startSeed
  where
    nextDay = extend $ rule config

-- |Create start grid with all default cells except the central one.
startGrid :: Config -> Int -> Comonad19Grid
startGrid config startSeed = Grid $ LZ (listSimpleLZ (-2 * startSeed - 2) (-1))
                                       (LZ (defaults (startSeed - 1) (-1)) (focus startSeed) $ defaults (startSeed + 1) 1)
                                       (listSimpleLZ (2 * startSeed + 2) 1)
  where
    simple seed = Cell Healthy $ mkStdGen seed

    defaults seed shift = simple seed : defaults (seed + shift) shift

    focus seed = Cell (Infected $ incubationPeriod config) $ mkStdGen seed
    listSimpleLZ i shift = LZ (defaults (i - 1) (-1)) (simple i) (defaults (i + 1) 1) : listSimpleLZ (i + shift) shift

-- |Print grid as field n*n, where:
--  '_' - healthy human;
--  'i' - infected (but not ill) human;
--  '#' - ill human;
--  '@' - immune human;
printGrid :: Int           -- ^ size
          -> Comonad19Grid -- ^ calculated grid
          -> IO ()         -- ^ out (in format '__i__'
                           -- ^                '@___#') with up and down barriers.
printGrid n grid = do
  putStrLn "*----------------------------------------------------------------------------------------------------------*"
  forM_ (grToList n grid) $ putStrLn . concatMap show
  putStrLn "*----------------------------------------------------------------------------------------------------------*"
  putStrLn ""
