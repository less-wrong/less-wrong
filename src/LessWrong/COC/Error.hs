module LessWrong.COC.Error where

import Data.Text (Text)
import LessWrong.COC.Type

data CalculusError = ParsingError Text
                   | UniverseHasNoType Const
                   | CannotEqualize Term Term
  deriving Show
