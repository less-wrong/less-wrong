{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
module LessWrong.COC.Type where

import           Data.String (IsString)
import           Data.Text   (Text)

-- |Indexed universe
data Uni = Star | Box Int
  deriving (Show, Read, Eq, Ord)

type Name = Text

-- |Variable names
newtype Var = V Name
  deriving (Show, Read, Eq, Ord, IsString)

-- |CoC term with infinity cumulative universes
data Term = Uni { uni  :: Uni  } -- ^Cumulative universes: *-{n}
          | Var { var  :: Var  } -- ^Variables: x
          | App { alg  :: Term
                , dat  :: Term } -- ^Application: M N
          | Lam { var  :: Var
                , tpe  :: Term
                , body :: Term } -- ^Abstraction: \(x : A) -> B
          | Pi  { var  :: Var
                , tpe  :: Term
                , body :: Term } -- ^Quantification: forall (x : A) -> B | A -> B ( == forall (_ : A) -> B)
  deriving (Show, Read)

-- |Variable with no name (wildcard variable)
noname :: Var
noname = "_"

-- |Cumulative principal of universes
axiom :: Uni -> Uni
axiom Star    = Box 1
axiom (Box i) = Box (i + 1)
