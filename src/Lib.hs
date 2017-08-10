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
import           Data.List                  (break, deleteBy, dropWhile,
                                             dropWhileEnd, foldl', isPrefixOf)
import           Data.Text                  (pack, unpack)
import           System.Console.Haskeline

import           LessWrong.COC.Check
import           LessWrong.COC.Error
import           LessWrong.COC.Eval
import           LessWrong.COC.Parser
import           LessWrong.COC.Pretty
import           LessWrong.COC.Type

import           LessWrong.HL.Parser        (parseREPL)
import           LessWrong.HL.Parser.Type   (Interactive (..), Declaration (..))
import           LessWrong.HL.Encoding      (encodeInductive)
import           LessWrong.HL.Type          (Decl (..), BoundTerm (..))

type TermContext = [(Var, Term)]

construct :: TermContext -> Term -> Term
construct = flip $ foldl' (\acc (n, t) -> substitute acc n t)

modifyCtx :: Monad m => Var -> Term -> StateT TermContext m ()
modifyCtx v t = do
    ctx <- get
    put $ newCtx ctx v t

newCtx :: TermContext -> Var -> Term -> TermContext
newCtx ctx v t = (v, t) : deleteBy (\x y -> fst x == fst y) (v, undefined) ctx

process :: Interactive -> StateT TermContext (InputT IO) ()
process (CODE term) = do
    ctx <- get
    let term' = construct ctx term
    case runExcept (typeOf term') of
      Right termType -> lift $ printTermAndType "_" (reduce term') termType
      Left  err      -> lift $ outputStrLn $ "Type error: " ++ show err
process (BIND var@(V name) term) = do
    ctx <- get
    let term' = construct ctx term
    case runExcept (typeOf term') of
      Right termType -> do
          modifyCtx var term'
          lift $ printTermAndType  name (reduce term') termType
      Left err       -> lift $ outputStrLn $ "Type error: " ++ show err
process (DECL (Ind decl)) = do
  ctx <- get
  let (Decl t cs)      = encodeInductive decl
  let tpeBT            = BT (bindingName t) (construct ctx $ bindingTerm t)
  let ctxT             = newCtx ctx (V $ bindingName tpeBT) (bindingTerm tpeBT)
  let (ctxTC, consBTs) = foldl (\(ctx', lst') bt -> second (:lst') $ btByCtx bt ctx') (ctxT, []) cs
  storeDecl (Decl tpeBT (reverse consBTs)) ctxTC

storeDecl :: Decl -> TermContext -> StateT TermContext (InputT IO) ()
storeDecl (Decl t cs) ctx =
  case runExcept (traverse (\(BT n t) -> typeOf t) (t:cs)) of
    Right (tt:cts) -> do
      put ctx
      lift $ printTermAndType (bindingName t) (bindingTerm t) tt
      forM_ (zip cts cs) $ \(termType, BT cn ct) ->
        lift $ printTermAndType cn ct termType
    Left  err      -> lift $ outputStrLn $ "Type error: " ++ show err
  
btByCtx :: BoundTerm -> TermContext -> (TermContext, BoundTerm)
btByCtx (BT n t) ctx = let nbt  = BT n (construct ctx t)
                           ctx' = newCtx ctx (V $ bindingName nbt) (bindingTerm nbt)
                       in  (ctx', nbt)
  
printTermAndType :: Name -> Term -> Term -> InputT IO ()
printTermAndType name term termType = do
    outputStrLn $ unpack name ++ " :: " ++ pretty termType
    outputStrLn $ unpack name ++ " =  " ++ pretty term

repl' :: StateT TermContext (InputT IO) ()
repl' = do inputMb <- lift $ getInputLine "> "
           ctx <- get
           case inputMb of
             Nothing -> pure ()
             Just input -> do
               case parseREPL (pack input) of
                 Right res -> process res
                 Left  err -> lift $ outputStrLn $ "Error: " ++ show err
               repl'

repl :: IO ()
repl = runInputT defaultSettings $ evalStateT repl' []
