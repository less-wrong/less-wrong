{-# LANGUAGE RecordWildCards #-}
module LessWrong.COC.Pretty where

import Data.Text
import LessWrong.COC.Type

pretty :: Term -> String
pretty (Const Star) = "*"
pretty (Const Box)  = "☐"
pretty (Var (V x))  = unpack x
pretty Lam{..}      = "λ(" ++ pretty (Var var) ++ " : " ++ pretty tpe ++ ") -> " ++ pretty body
pretty Pi{..}       | var == noname = pretty tpe ++ " -> " ++ pretty body
                    | otherwise = "∀(" ++ pretty (Var var) ++ " : " ++ pretty tpe ++ ") -> " ++ pretty body
pretty App{..}      = pretty alg ++ " " ++ prettyDat
  where prettyDat = case dat of
                      Const{} -> pretty dat
                      Var{}   -> pretty dat
                      _       -> "(" ++ pretty dat ++ ")"
