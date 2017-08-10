module LessWrong.HL.Type where

import LessWrong.COC.Type (Name, Term)
import LessWrong.COC.Eval ()

data BoundTerm = BT { bindingName :: Name
                    , bindingTerm :: Term
                    }
  deriving (Show, Eq)

data Decl = Decl { typeBinding   :: BoundTerm
                 , consesBinding :: [BoundTerm]
                 }
  deriving (Show, Eq)

data FuncDecl = FuncDecl { funcName :: Name
                         , funcType :: Term
                         , funcTerm :: Term
                         }
  deriving (Show, Eq)
