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
typeWith _   Uni{..}   = pure $ Uni (axiom uni)
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
                            pure $ Uni (typeRule aTpe bTpe)
typeWith ctx App{..}   = do let inctx = inclusion ctx
                            algTpe <- reduce <$> typeWith ctx (reduce alg)
                            case algTpe of
                              Pi{..} -> pure ()
                              _      -> throwE $ InvalidType algTpe "not a Pi-type in application algo"
                            datTpe <- reduce <$> typeWith ctx dat
                            unless (datTpe `inctx` tpe algTpe) $
                              throwE $ CannotEqualize datTpe (tpe algTpe)
                            pure $ substitute (body algTpe) (var algTpe) dat

-- |Every object of 'Star' is an object of 'Box'{i}
--  and every object of 'Box'{i} is an object of 'Box'{j} for every i <= j
inclusion :: Context Term -> Term -> Term -> Bool
inclusion _   (Uni x) (Uni y)            = x <= y
inclusion _   (Var x) (Var y)            = x == y
inclusion ctx (Var x) u@Uni{}            = case lookup x ctx of
                                             Just k  -> inclusion ctx k u
                                             Nothing -> False
inclusion ctx (App e1 d1) (App e2 d2)    = inclusion ctx e1 e2 && inclusion ctx d1 d2
inclusion ctx (Pi v t b) (Pi v' t' b')   = inclusion ctx t t' && inclusion ctx b (substitute b' v' (Var v))
inclusion ctx (Lam v t b) (Lam v' t' b') = inclusion ctx t t' && inclusion ctx b (substitute b' v' (Var v))
inclusion _   _ _                        = False

typeConst :: Term -> Except CalculusError Uni
typeConst (Uni x) = pure x
typeConst a       = throwE $ InvalidType a "non-universe type"

typeRule :: Uni -> Uni -> Uni
typeRule _       Star    = Star
typeRule Star    uni     = uni
typeRule (Box i) (Box j) = Box (max i j)
