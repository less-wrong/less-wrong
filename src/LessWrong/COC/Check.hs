{-# LANGUAGE RecordWildCards #-}
module LessWrong.COC.Check where

import           Prelude               hiding (lookup)

import           LessWrong.COC.Context
import           LessWrong.COC.Error
import           LessWrong.COC.Eval    ()
import           LessWrong.COC.Type

typeOf :: Term -> Either CalculusError Term
typeOf = typeWith empty

typeWith :: Context Term -> Term -> Either CalculusError Term
typeWith _   Const{..} | uni == Star = pure $ Const Box
                       | otherwise = Left $ UniverseHasNoType Box
typeWith ctx Var{..}   = case lookup var ctx of
                           Just tpe -> pure tpe
                           Nothing  -> Left $ UnknownVariable var
typeWith ctx Lam{..}   = do let ctx' = insert var tpe ctx
                            bodyTpe <- typeWith ctx' body
                            let lamTpe = Pi var tpe bodyTpe
                            _ <- typeWith ctx lamTpe -- check that type is well-typed
                            pure lamTpe
typeWith ctx Pi{..}    = undefined
typeWith ctx App{..}   = undefined

typeCheck :: Term {- Type -} -> Term {- Term -} -> Either CalculusError ()
typeCheck tpe term =
  case typeOf term of
    Left err   -> Left err
    Right tpe' -> if tpe == tpe'
                  then pure ()
                  else Left $ CannotEqualize tpe tpe'
