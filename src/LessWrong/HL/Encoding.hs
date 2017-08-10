{-# LANGUAGE RecordWildCards #-}

module LessWrong.HL.Encoding where

import           Control.Arrow            (first)
import           Data.Set                 (Set, insert, member, singleton)

import           LessWrong.COC.Context    (Context (..))
import           LessWrong.COC.Eval       (alpha, free)
import           LessWrong.COC.Type       (Name, Term (..), Uni (..), Var (..),
                                           noname)
import           LessWrong.HL.Parser.Type (Inductive (..))
import           LessWrong.HL.Type        (BoundTerm (..), Decl (..))

encodeInductive :: Inductive -> Decl
encodeInductive Inductive{..} = Decl typeBT consesBTs
  where
    constructors = getCtx indConses
    parameters   = getCtx indParams

    consesBTs :: [BoundTerm]
    consesBTs = uncurry makeConstructor <$> constructors

    typeBT :: BoundTerm
    typeBT = BT indName typeTerm
      where
        typeTerm = foldToQua Lam parameters $ foldToT $ foldToQua Pi constructors typeFullTerm

    makeConstructor :: Var -> Term -> BoundTerm
    makeConstructor (V name) term = BT name cons
      where
        cons = let preCons     = piToLam $ foldToT $ foldToQua Pi constructors $ foldedApp name deRecArgs
                   nParameters = filter ((`member` free preCons) . fst) parameters
                   args        = extractArgs $ nameArgs term
                   deRecArgs   = foldRec <$> args
               in  foldToQua Lam nParameters $ foldToQua Lam args preCons

        nameArgs :: Term -> Term
        nameArgs term = nameArgsList term (singleton noname)
          where
            nameArgsList :: Term -> Set Var -> Term
            nameArgsList t@Pi{} set = let Pi{..} = alpha t set
                                      in  Pi var tpe (nameArgsList body (var `insert` set))
            nameArgsList t      _   = t

        extractArgs :: Term -> [(Var, Term)]
        extractArgs Pi{..} = (var, tpe) : extractArgs body
        extractArgs _      = []

        piToLam :: Term -> Term
        piToLam Pi{..} = Lam var tpe (piToLam body)
        piToLam t      = t

    typeFullTerm :: Term
    typeFullTerm = foldedApp indName $ first Var <$> parameters

    foldRec :: (Var, Term) -> (Term, Term)
    foldRec (V v, t) | t == typeFullTerm =
      let foldList = first Var <$> (V indName, undefined) : constructors
      in  (foldedApp v foldList, t)
    foldRec (v, t)                       = (Var v, t)

    foldedApp :: Name -> [(Term, Term)] -> Term
    foldedApp n = foldl (\t (v, _) -> App t v) (Var $ V n)

    foldToT :: Term -> Term
    foldToT = Pi (V indName) (foldToQua Pi parameters (Uni Star))

    foldToQua :: (Var -> Term -> Term -> Term) -> [(Var, Term)] -> Term -> Term
    foldToQua f ctx term = foldr (\(v,t) b -> f v t b) term ctx
