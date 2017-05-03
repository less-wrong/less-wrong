module LessWrong.Term.Pretty where

import Data.Text (unpack)
import LessWrong.Term

data Position = AppAlgo | AppData | LamBody | Unknown

pretty :: Term -> String
pretty = pretty' Unknown Unknown

pretty' :: Position -> Position -> Term -> String
pretty' LamBody _ (Var x)     = '.' : unpack x
pretty' _       _ (Var x)     = unpack x
pretty' i       _ (App e1 e2) = let s = (pretty' AppAlgo i e1) ++ " " ++ (pretty' AppData i e2)
                                in case i of
                                     LamBody -> '.' : s
                                     AppData -> "(" ++ s ++ ")"
                                     _       -> s
pretty' i       p (Lam x e)   = let s = unpack x ++ pretty' LamBody i e
                                in case i of
                                     Unknown -> 'λ' : s
                                     LamBody -> ' ' : s
                                     AppAlgo -> "(λ" ++ s ++ ")"
                                     AppData ->
                                       case p of
                                         AppAlgo -> "(λ" ++ s ++ ")"
                                         _       -> 'λ' : s
