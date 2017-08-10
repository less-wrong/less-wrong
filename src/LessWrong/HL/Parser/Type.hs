module LessWrong.HL.Parser.Type where

import           LessWrong.COC.Context (Context (..))
import           LessWrong.COC.Eval    ()
import           LessWrong.COC.Type    (Name, Term (..), Var (..))

data Inductive = Inductive { indName   :: Name
                           , indParams :: Context Term
                           , indConses :: Context Term
                           }
  deriving (Show, Eq)

data TypeApp = TVar Name
             | TApp TypeApp TypeApp
  deriving (Show, Read, Eq, Ord)

data Algebraic = Algebraic { algName   :: Name
                           , algParams :: [Name]
                           , algConses :: Context [TypeApp]
                           }
  deriving (Show, Eq)

data Record = Record { recName   :: Name
                     , recParams :: Context Term
                     , recConses :: Context Term
                     }
  deriving (Show, Eq)

data Declaration = Ind Inductive | Alg Algebraic | Rec Record
  deriving (Show, Eq)

data Interactive = DECL Declaration
                 | BIND Var Term
                 | CODE Term
  deriving (Show, Eq)
