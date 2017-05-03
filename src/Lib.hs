module Lib
    ( repl
    ) where

import           Control.Monad
import           Data.Text                (pack)
import           System.Console.Haskeline

import           LessWrong.Term
import           LessWrong.Term.Parser
import           LessWrong.Term.Pretty

repl' :: InputT IO ()
repl' = do inputMb <- getInputLine "> "
           case inputMb of
             Nothing -> pure ()
             Just input -> do
               case parseTerm (pack input) of
                 Right term -> do
                   outputStrLn $ "Term:    " ++ pretty term
                   outputStrLn $ "Reduced: " ++ pretty (reduce term)
                 Left err ->
                   outputStrLn $ show err
               repl'

repl :: IO ()
repl = runInputT defaultSettings repl'
