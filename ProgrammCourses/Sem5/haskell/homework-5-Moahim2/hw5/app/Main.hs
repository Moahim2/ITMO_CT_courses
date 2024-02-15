module Main (main) where

import HW5.Action
import HW5.Evaluator (eval)
import HW5.Parser (parse)
import HW5.Pretty (prettyValue)

import System.Console.Haskeline
import GHC.Exts (fromList)
import Control.Monad.Cont (liftIO)

-- |Aka repl the interpreter.
-- It is closes if empty or ":q!" input else it does full Hi session
-- (parse + eval + pretty or show Exception if there was an error at some stage).
main :: IO ()
main = runInputT defaultSettings loop
  where
    loop = do
       minput <- getInputLine "hi> "
       case minput of
         Nothing -> return ()
         Just ":q!" -> return ()
         Just input -> do
           case parse input of
             Left parseError -> loopPrint parseError
             Right hiExpr    -> do
               hiValueEither <- liftIO . runHIO (eval hiExpr) $ fromList [minBound..maxBound]
               case hiValueEither of
                 Left evalError -> loopPrint evalError
                 Right hiValue  -> loopPrint $ prettyValue hiValue
           loop

    loopPrint :: Show a => a -> InputT IO ()
    loopPrint = outputStrLn . show
