module Lib
    ( repl
    ) where

import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Text                (pack)
import           System.Console.Haskeline

import           LessWrong.COC.Type
import           LessWrong.COC.Eval
import           LessWrong.COC.Parser
import           LessWrong.COC.Check
import           LessWrong.COC.Pretty

repl' :: InputT IO ()
repl' = do inputMb <- getInputLine "> "
           case inputMb of
             Nothing -> pure ()
             Just input -> do
               case parseTerm (pack input) of
                 Right term -> do
                   outputStrLn $ "Term:    " ++ pretty term
                   let rterm = reduce term
                   outputStrLn $ "Reduced: " ++ pretty rterm
                   case runExcept (typeOf term) of
                     Right tpe -> outputStrLn $ "Type:    " ++ pretty tpe
                     Left  err -> outputStrLn $ "Error:   " ++ show err
                 Left err ->
                   outputStrLn $ show err
               repl'

repl :: IO ()
repl = runInputT defaultSettings repl'
