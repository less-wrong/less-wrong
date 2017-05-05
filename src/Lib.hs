module Lib
    ( repl
    ) where

import           Control.Monad
import           Data.Text                (pack)
import           System.Console.Haskeline

import           LessWrong.COC.Type
import           LessWrong.COC.Eval
import           LessWrong.COC.Parser

repl' :: InputT IO ()
repl' = do inputMb <- getInputLine "> "
           case inputMb of
             Nothing -> pure ()
             Just input -> do
               case parseTerm (pack input) of
                 Right term -> do
                   outputStrLn $ "Term:    " ++ show term
                   let rterm = reduce term
                   outputStrLn $ "Reduced: " ++ show rterm
                 Left err ->
                   outputStrLn $ show err
               repl'

repl :: IO ()
repl = runInputT defaultSettings repl'
