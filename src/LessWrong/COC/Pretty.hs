{-# LANGUAGE RecordWildCards #-}
module LessWrong.COC.Pretty where

import           Data.Set           (notMember)
import           Data.Text
import           LessWrong.COC.Eval
import           LessWrong.COC.Type

pretty :: Term -> String
pretty (Uni Star)    = "*"
pretty (Uni (Box i)) | i == 1    = "◻"
                     | otherwise = "◻-" ++ show i
pretty (Var (V x))   = unpack x
pretty Lam{..}       = "λ(" ++ pretty (Var var) ++ " : " ++ pretty tpe ++ ") -> " ++ pretty body
pretty Pi{..}        = prettyPi var tpe body
pretty App{..}       = pretty alg ++ " " ++ prettyDat
  where prettyDat = case dat of
                      Uni{}   -> pretty dat
                      Var{}   -> pretty dat
                      _       -> "(" ++ pretty dat ++ ")"

prettyPi :: Var -> Term -> Term -> String
prettyPi v t b | v `notMember` free b = case t of
                                          Pi{}  -> "(" ++ pretty t ++ ") -> " ++ pretty b
                                          Lam{} -> "(" ++ pretty t ++ ") -> " ++ pretty b
                                          _      -> pretty t ++ " -> " ++ pretty b
               | otherwise = "∀(" ++ pretty (Var v) ++ " : " ++ pretty t ++ ") -> " ++ pretty b
