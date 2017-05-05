module LessWrong.COC.Error where

import LessWrong.COC.Type

data CalculusError = ParsingError String
                   | UniverseHasNoType Const
                   | CannotEqualize Term Term
                   | UnknownVariable Var

instance Show CalculusError where
  show (ParsingError txt) = "Parsing error:\n" ++ txt
  show _ = "No show yet"
