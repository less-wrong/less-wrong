module Lib
    ( repl
    ) where

import           Control.Arrow              (second)
import           Control.Monad
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.State
import           Data.Char                  (isSpace)
import           Data.List                  (break, dropWhile, dropWhileEnd,
                                             foldl', isPrefixOf)
import           Data.Text                  (pack)
import           System.Console.Haskeline

import           LessWrong.COC.Check
import           LessWrong.COC.Error
import           LessWrong.COC.Eval
import           LessWrong.COC.Parser
import           LessWrong.COC.Pretty
import           LessWrong.COC.Type

type TermContext = [(Var, Term)]

termAndType :: String -> TermContext -> Either CalculusError (Term, Term)
termAndType txt ctx =
  case parseTerm (pack txt) of
    Right term' -> do let term = foldl' (\acc (n, t) -> substitute acc n t) term' ctx
                      case runExcept (typeOf term) of
                        Right tpe -> pure (reduce term, tpe)
                        Left err  -> Left err
    Left err    -> Left err

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace

processInput :: String -> StateT TermContext (InputT IO) (String, String, Term -> StateT TermContext (InputT IO) ())
processInput input =
  case isPrefixOf ":let " input of
    True -> do let (nameS, termS) = second tail $ break (== '=') $ drop 5 input
               let name' = strip nameS
               let term' = strip termS
               pure (name', term', \term -> modify ((V $ pack name', term):))
    False -> pure ("_", input, const $ pure ())

repl' :: StateT TermContext (InputT IO) ()
repl' = do inputMb <- lift $ getInputLine "> "
           ctx <- get
           case inputMb of
             Nothing -> pure ()
             Just input -> do
               (name, termS, foo) <- processInput input
               case termAndType termS ctx of
                 Right (term, tpe) -> do
                   foo term
                   lift $ outputStrLn $ name ++ " :: " ++ pretty tpe
                   lift $ outputStrLn $ name ++ " =  " ++ pretty term
                 Left err ->
                   lift $ outputStrLn $ "Error: " ++ show err
               repl'

repl :: IO ()
repl = runInputT defaultSettings $ evalStateT repl' []
