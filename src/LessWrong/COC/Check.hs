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
typeWith ctx App{..}   = do let inctx = inc ctx
                            algTpe <- reduce <$> typeWith ctx (reduce alg)
                            case algTpe of
                              Pi{..} -> pure ()
                              _      -> throwE $ InvalidType algTpe "not a Pi-type in application algo"
                            datTpe <- reduce <$> typeWith ctx dat
                            unless (datTpe `inctx` tpe algTpe) $
                              throwE $ CannotEqualize datTpe (tpe algTpe)
                            pure $ substitute (body algTpe) (var algTpe) dat

typeConst :: Term -> Except CalculusError Uni
typeConst (Uni x) = pure x
typeConst a       = throwE $ InvalidType a "non-universe type"

-- |Every object in 'Star' is an object in 'Box'{i}
--  and every object in 'Box'{i} is an object in 'Box'{j} for every i <= j
inc :: Context Term -> Term -> Term -> Bool
inc _   (Uni x) (Uni y)            = x <= y
inc _   (Var x) (Var y)            = x == y
inc ctx (Var x) u@Uni{}            = case lookup x ctx of
                                       Just k  -> inc ctx k u
                                       Nothing -> False
inc ctx (App e1 d1) (App e2 d2)    = inc ctx e1 e2 && inc ctx d1 d2
inc ctx (Pi v t b) (Pi v' t' b')   = inc ctx t t' && inc ctx b (substitute b' v' (Var v))
inc ctx (Lam v t b) (Lam v' t' b') = inc ctx t t' && inc ctx b (substitute b' v' (Var v))
inc _   _ _                        = False

typeRule :: Uni -> Uni -> Uni
typeRule _       Star    = Star
typeRule Star    uni     = uni
typeRule (Box i) (Box j) = Box (max i j)
