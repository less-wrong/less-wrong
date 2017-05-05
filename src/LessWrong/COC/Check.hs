module LessWrong.COC.Check where

import LessWrong.COC.Type
import LessWrong.COC.Context
import LessWrong.COC.Error

typeOf :: Term -> Either CalculusError Term
typeOf term = undefined

typeCheck :: Term {- Type -} -> Term {- Term -} -> Either CalculusError ()
typeCheck tpe term =
  case typeOf term of
    Left err   -> Left err
    Right tpe' -> if tpe == tpe'
                  then pure ()
                  else Left $ CannotEqualize tpe tpe'
