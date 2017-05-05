module LessWrong.COC.Context where

import           LessWrong.COC.Type
import           Prelude            hiding (lookup)
import qualified Prelude            as P (lookup)

newtype Context a = Context { getCtx :: [(Var, a)] }

empty :: Context a
empty = Context []

insert :: Var -> a -> Context a -> Context a
insert k v = Context . ((k,v):) . getCtx

lookup :: Var -> Context a -> Maybe a
lookup k = P.lookup k . getCtx
