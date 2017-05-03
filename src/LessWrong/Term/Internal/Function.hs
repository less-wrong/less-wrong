{-# LANGUAGE RecordWildCards #-}

module LessWrong.Term.Internal.Function where

import           Data.Set                     (Set, delete, empty, insert,
                                               member, notMember, singleton,
                                               union)
import           Data.Text                    (pack)
import           LessWrong.Term.Internal.Type

free :: Term -> Set Name
free Var{..} = singleton var
free App{..} = free algo `union` free arg
free Lam{..} = variable `delete` free body

bound :: Term -> Set Name
bound Var{..} = empty
bound App{..} = bound algo `union` bound arg
bound Lam{..} = variable `insert` free body

substitute :: Term -> Name -> Term -> Term
substitute v@Var{..} n e | var == n  = e
                         | otherwise = v
substitute   App{..} n e = App (substitute algo n e) (substitute arg n e)
substitute l@Lam{..} n e | variable == n = l
                         | otherwise     = let cond   = variable `member` free e
                                               a_lam  = alpha l (free e)
                                               s_body = substitute body n e
                                           in if cond then substitute a_lam n e
                                                      else Lam variable s_body

rename :: Set Name -> Name
rename conflicts = head . dropWhile (`member` conflicts) $ fmap pack $ primitive ++ numeric
  where letters   = ['a'..'z']
        primitive = fmap (:[]) letters
        numeric   = do number <- show <$> ([1..] :: [Int])
                       letter <- letters
                       pure (letter:number)

alpha :: Term -> Set Name -> Term
alpha Lam{..} conflicts | hasConflict = let all_conflicts = conflicts `union` bound body
                                            n_variable = rename all_conflicts
                                            n_body = substitute body variable (Var n_variable)
                                        in Lam n_variable n_body
                        | otherwise   = Lam variable (alpha body conflicts)
                      where hasConflict = variable `member` conflicts
alpha App{..} conflicts = App (alpha algo conflicts) (alpha arg conflicts)
alpha var _ = var

beta :: Term -> Term
beta (App (Lam n e1) e2) = substitute e1 n e2
beta App{..} = let b_algo = beta algo
               in if b_algo /= algo then App b_algo arg else App algo (beta arg)
beta (Lam n e) = Lam n (beta e)
beta var = var

eta :: Term -> Term
eta l@(Lam v (App algo (Var e))) | hasEta    = algo
                                 | otherwise = l
                               where hasEta = v == e && v `notMember` free algo
eta term = term

redexCount :: Term -> Int
redexCount Lam{..}                = redexCount body
redexCount Var{..}                = 0
redexCount (App (Lam _ body) arg) = 1 + redexCount body + redexCount arg
redexCount App{..}                = redexCount algo + redexCount arg

reduce :: Term -> Term
reduce term = let b_term  = beta term
                  r1_term = if b_term /= term then reduce b_term else term
                  e_term  = eta r1_term
                  r2_term = if e_term /= r1_term then reduce e_term else r1_term
              in eta r2_term
