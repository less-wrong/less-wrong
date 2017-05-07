module LessWrong.COC.Type where

import Data.Text

data Const = Star | Box
  deriving (Show, Read, Eq, Ord, Bounded, Enum)

type Name = Text

data Var = V Name
  deriving (Show, Read, Eq, Ord)

data Term = Const { uni  :: Const } -- * | □
          | Var   { var  :: Var   } -- x
          | App   { alg  :: Term
                  , dat  :: Term  } -- M N
          | Lam   { var  :: Var
                  , tpe  :: Term
                  , body :: Term  } -- \(x : A) -> B
          | Pi    { var  :: Var
                  , tpe  :: Term
                  , body :: Term  } -- forall (x : A) -> B | A -> B
  deriving (Show, Read)

noname :: Var
noname = V (pack "_")
