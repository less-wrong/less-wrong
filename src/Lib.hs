{-# LANGUAGE OverloadedStrings #-}

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
                                             foldl', isPrefixOf, deleteBy)
import           Data.Text                  (pack)
import           System.Console.Haskeline

import           LessWrong.COC.Check
import           LessWrong.COC.Error
import           LessWrong.COC.Eval
import           LessWrong.COC.Parser
import           LessWrong.COC.Pretty
import           LessWrong.COC.Type

type TermContext = [(Var, Term)]

construct :: TermContext -> Term -> Term
construct = flip $ foldl' (\acc (n, t) -> substitute acc n t)

termAndType :: String -> TermContext -> Either CalculusError (Term, Term)
termAndType txt ctx =
  case parseTerm (pack txt) of
    Right term' -> do let term = construct ctx term'
                      case runExcept (typeOf term) of
                        Right tpe -> pure (reduce term, tpe)
                        Left err  -> Left err
    Left err    -> Left err

strip :: String -> String
strip = dropWhile isSpace . dropWhileEnd isSpace

modifyCtx :: Monad m => Var -> Term -> StateT TermContext m ()
modifyCtx v t = do ctx <- get
                   put $ (v, t) : deleteBy (\x y -> fst x == fst y) (v, undefined) ctx

processInput :: Monad m => String -> StateT TermContext m (String, String, Term -> StateT TermContext m ())
processInput input =
  if ":let " `isPrefixOf` input
    then do let (nameS, termS) = second tail $ break (== '=') $ drop 5 input
            let name' = strip nameS
            pure (name', strip termS, modifyCtx (V $ pack name'))
    else pure ("_", input, const $ pure ())

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
