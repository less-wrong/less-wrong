module LessWrong.Term.Internal.Type where

import Data.Text

type Name = Text

data Term = Var { var :: Name }
          | App { algo :: Term, arg :: Term }
          | Lam { variable :: Name, body :: Term }
  deriving (Show, Eq, Ord)
