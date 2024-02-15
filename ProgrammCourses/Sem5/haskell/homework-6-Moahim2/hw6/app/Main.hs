module Main (main) where

import HW6.T3 (Config (..), simulate, printGrid)

import Control.Monad (forM_, when)

import Options.Applicative (Parser, execParser, info, (<**>), helper,
                            fullDesc, option, auto, long, help, showDefault, value, metavar)

data Parameters = Parameters
  { prob  :: Double
  , incub :: Int
  , ill   :: Int
  , immun :: Int
  , size  :: Int
  , count :: Int
  }

parameters :: Parser Parameters
parameters = Parameters
           <$> option auto
               ( long "prob"
              <> help "Propability of infected"
              <> showDefault
              <> value 1.0                     --default value
              <> metavar "DOUBLE" )
           <*> option auto
              ( long "incub"
              <> help "Incubation period"
              <> showDefault
              <> value 2
              <> metavar "INT" )
           <*> option auto
               ( long "ill"
              <> help "Illness period"
              <> showDefault
              <> value 5
              <> metavar "INT" )
           <*> option auto
               ( long "immun"
              <> help "Immunity period"
              <> showDefault
              <> value 0
              <> metavar "INT" )
           <*> option auto
               ( long "grid-size"
              <> help "Size of grid (n * n)"
              <> showDefault
              <> value 6
              <> metavar "INT" )
           <*> option auto
               ( long "iterations"
              <> help "Count of iterations"
              <> showDefault
              <> value 3
              <> metavar "INT" )   

main :: IO ()
main = do
    p <- execParser opts
    validate (0 > prob p || prob p > 1) "Prob must be in range [0, 1]."
    validate (incub p < 1) "Incub must be > 0."
    validate (ill p < 1) "Ill must be > 0."
    validate (immun p < 1) "Immun must be > 0."
    validate (size p < 1) "Grid-size must be > 0."
    validate (count p < 0) "Iterations must be non negative."
    forM_ (take (count p) $ simulate (Config (prob p) (incub p) (ill p) $ immun p) 42) $ printGrid $ size p
  where
    opts = info (parameters <**> helper) fullDesc

    validate badCondition message = when badCondition $ error message
