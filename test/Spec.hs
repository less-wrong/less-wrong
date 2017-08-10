{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}

data Nonrecursive a b c = A | B a | C b c
  deriving (Show, Eq, Ord)

type NR a b c = forall x. x -> (a -> x) -> (b -> c -> x) -> x

a :: NR a b c
a       = \a' _ _ -> a'

b :: a -> NR a b c
b xa    = \_ b' _ -> b' xa

c :: b -> c -> NR a b c
c xb xc = \_ _ c' -> c' xb xc

--

data WL a = WN | WC a (WL Int)
  deriving (Show, Eq, Ord)

type WL' a = forall x. x a -> (a -> x Int -> x a) -> x a

--

main :: IO ()
main = pure ()
