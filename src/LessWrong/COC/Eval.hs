{-# LANGUAGE RecordWildCards #-}

module LessWrong.COC.Eval where

import           Prelude hiding     (length)
import           Data.Set           (Set, delete, empty, insert, member,
                                     notMember, singleton, union)
import           Data.Text          (pack)
import           LessWrong.COC.Type

instance Eq Term where
  Uni a == Uni b             = a == b
  Var a == Var b             = a == b
  App a b == App a' b'       = a == a' && b == b'
  Pi v t b == Pi v' t' b'    = t == t' && b == substitute b' v' (Var v)
  Lam v t b == Lam v' t' b'  = t == t' && b == substitute b' v' (Var v)
  _ == _                     = False

free :: Term -> Set Var
free term =
  case term of
    Uni{} -> empty
    Var{..} -> singleton var
    App{..} -> free alg `union` free dat
    Lam{..} -> del
    Pi{..}  -> del
  where del = delete (var term) $ free (body term) `union` free (tpe term)

bound :: Term -> Set Var
bound term =
  case term of
    Uni{} -> empty
    Var{}   -> empty
    App{..} -> bound alg `union` bound dat
    Lam{..} -> ins
    Pi{..}  -> ins
  where ins | var term == noname = bound (body term)
            | otherwise          = var term `insert` (bound (body term) `union` bound (tpe term))

freshName :: Set Var -> Var
freshName conflicts = head . dropWhile (`member` conflicts) $ fmap (V . pack) $ primitive ++ numeric
  where letters   = ['a'..'z']
        primitive = fmap (:[]) letters
        numeric   = do number <- show <$> ([1..] :: [Int])
                       letter <- letters
                       pure (letter:number)

reduce :: Term -> Term
reduce term | beta term == term = eta term
            | otherwise         = reduce (beta term)

alpha :: Term -> Set Var -> Term
alpha term conflicts =
  case term of
    Uni{}                 -> term
    Var{}                 -> term
    Lam{..} | hasConflict -> Lam var' tpe body'
            | otherwise   -> Lam var tpe (alpha body conflicts)
    Pi{..}  | hasConflict -> Pi var' tpe body'
            | otherwise   -> Pi var tpe (alpha body conflicts)
    App{..}               -> App (alpha alg conflicts) (alpha dat conflicts)
  where hasConflict = var term `member` conflicts
        conflicts'  = free (body term) `union` free (tpe term) `union` conflicts
        var'        = freshName conflicts'
        body'       = substitute (body term) (var term) (Var var')

beta :: Term -> Term
beta term =
  case term of
    Uni{}   -> term
    Var{}   -> term
    App{..} -> case alg of
                 Lam{..} -> substitute body var dat
                 _       -> let alg' = beta alg
                                dat' = beta dat
                            in if alg' /= alg
                               then App alg' dat
                               else App alg dat'
    Lam{..} -> mkBeta Lam
    Pi{..}  -> mkBeta Pi
  where mkBeta :: (Var -> Term -> Term -> Term) -> Term
        mkBeta f = let tpe'  = beta $ tpe term
                       body' = beta $ body term
                   in if tpe' /= tpe term
                      then f (var term) tpe' (body term)
                      else f (var term) (tpe term) body'

eta :: Term -> Term
eta term =
  case term of
    Uni{}   -> term
    Var{}   -> term
    Lam{..} -> case body of
                 App alg (Var x) | x == var && x `notMember` free alg -> eta alg
                 _ -> Lam var (eta tpe) (eta body)
    Pi{..}  -> Pi  var (eta tpe) (eta body)
    App{..} -> App (eta alg) (eta dat)

substitute :: Term -> Var -> Term -> Term -- a[x := b]
substitute a x b =
  case a of
    Uni{..}             -> a
    Var{..} | var == x  -> b
            | otherwise -> a
    App{..}             -> App (substitute alg x b) (substitute dat x b)
    Lam{..} | var == x  -> a
            | otherwise -> if cond then substitute a_alp x b
                                   else Lam var tpe' body'
    Pi{..}  | var == x  -> a
            | otherwise -> if cond then substitute a_alp x b
                                   else Pi var tpe' body'
  where cond = var a `member` free b
        a_alp = alpha a (free b)
        tpe'  = substitute (tpe a) x b
        body' = substitute (body a) x b
