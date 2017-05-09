module LessWrong.COC.Error where

import           Data.Text          (unpack)
import           LessWrong.COC.Type
import           LessWrong.COC.Pretty

data CalculusError = ParsingError String
                   | UniverseHasNoType Const
                   | UnknownVariable Var
                   | CannotEqualize Term Term
                   | InvalidType Term String

instance Show CalculusError where
  show (ParsingError txt)      = "Parsing error:\n" ++ txt
  show (UniverseHasNoType ct)  = "Universe " ++ show ct ++ " cannot be typed"
  show (UnknownVariable (V v)) = "Variable '" ++ unpack v ++ "' is not defined"
  show (CannotEqualize t t')   = "Cannot equalize types '" ++ pretty t ++ "' and '" ++ pretty t' ++ "'"
  show (InvalidType t r)       = "Type '" ++ pretty t ++ "' is invalid (reason: " ++ r ++ ")"
  
