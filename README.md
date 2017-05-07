less-wrong
==========

I hope one day a nice prover with tactics would be here, but not it is just a simple CoC calulator.

Examples
--------

**id**
```haskell
id :: a -> a
id x = x
```
is
```
\(a : *) -> \(x : a) -> x
```

**id id**
```haskell
id :: a -> a
id x = x

idid = id id
```
is
```
(\(id : forall (a : *) -> a -> a) -> id (forall (a : *) -> a -> a) id) (\(a : *) -> \(x : a) -> x)
```
and reduces to
```
id :: ∀(a : *) -> a -> a
id = λ(a : *) -> λ(x : a) -> x
```
