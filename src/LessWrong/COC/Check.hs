{-# LANGUAGE RecordWildCards #-}
module LessWrong.COC.Check where

import           Prelude                    hiding (lookup)

import           LessWrong.COC.Context
import           LessWrong.COC.Error
import           LessWrong.COC.Eval         (reduce, substitute)
import           LessWrong.COC.Type

import           Control.Monad
import           Control.Monad.Trans.Except

typeOf :: Term -> Except CalculusError Term
typeOf = typeWith empty

typeWith :: Context Term -> Term -> Except CalculusError Term
typeWith _   Const{..} | uni == Star = pure $ Const Box
                       | otherwise = throwE $ UniverseHasNoType Box
typeWith ctx Var{..}   = case lookup var ctx of
                           Just tpe -> pure tpe
                           Nothing  -> throwE $ UnknownVariable var
typeWith ctx Lam{..}   = do let ctx' = insert var tpe ctx
                            bodyTpe <- typeWith ctx' body
                            let lamTpe = Pi var tpe bodyTpe
                            _ <- typeWith ctx lamTpe -- check that type is well-typed
                            pure lamTpe
typeWith ctx Pi{..}    = do aTpe <- (reduce <$> typeWith ctx tpe) >>= typeConst
                            bTpe <- (reduce <$> typeWith (insert var tpe ctx) body) >>= typeConst
                            pure $ typeRule aTpe bTpe
typeWith ctx App{..}   = do algTpe <- reduce <$> typeWith ctx (reduce alg)
                            case algTpe of
                              Pi{..} -> pure ()
                              _      -> throwE $ InvalidType algTpe "not a Pi-type in application algo"
                            datTpe <- reduce <$> typeWith ctx dat
                            when (tpe algTpe /= datTpe) $
                              throwE $ CannotEqualize (tpe algTpe) datTpe
                            pure $ substitute (body algTpe) (var algTpe) dat

typeCheck :: Term {- Type -} -> Term {- Term -} -> Except CalculusError ()
typeCheck tpe term = do tpe' <- typeOf term
                        when (tpe /= tpe') $
                          throwE $ CannotEqualize tpe tpe'
                        pure ()

typeConst :: Term -> Except CalculusError Const
typeConst (Const x) = pure x
typeConst a         = throwE $ InvalidType a "non-universe type"

typeRule :: Const -> Const -> Term
typeRule Star Star = Const Star
typeRule Star Box  = Const Box
typeRule Box Star  = Const Star
typeRule Box Box   = Const Box
