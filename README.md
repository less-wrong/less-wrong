less-wrong
==========

[![Travis](https://img.shields.io/travis/zmactep/less-wrong.svg)](https://travis-ci.org/zmactep/less-wrong)
[![license](https://img.shields.io/github/license/zmactep/less-wrong.svg)]()

I hope one day a nice prover with tactics would be here, but now it is just a simple CoC calululator with one impredicative universe `*` and an infinity set of cumulative `◻-#i` universes. So, LW has these two axioms (I use `◻` for `◻-1`):
```
* : ◻
◻-i : ◻-i+1
```

By the cumulative principal all terms can be presented in higher order universes too:
```haskell
> :let id = \(a : ◻) -> a
id :: ◻ -> ◻
id =  λ(a : ◻) -> a
> \(a : ◻) -> id a
_ :: ◻ -> ◻
_ =  λ(a : ◻) -> a
> \(a : *) -> id a
_ :: * -> ◻
_ =  λ(a : *) -> a
> \(a : *) -> \(x : a) -> id x
_ :: ∀(a : *) -> a -> ◻
_ =  λ(a : *) -> λ(x : a) -> x
```

Inductive types
---------------

`less-wrong` supports inductive types by Church-Berrarduci-Boehm encoding to pure CoC:

### Booleans

``` haskell
> inductive Bool = (True : Bool) (False : Bool)
Bool :: *
Bool =  ∀(Bool : *) -> Bool -> Bool -> Bool
True :: ∀(Bool : *) -> Bool -> Bool -> Bool
True =  λ(Bool : *) -> λ(True : Bool) -> λ(False : Bool) -> True
False :: ∀(Bool : *) -> Bool -> Bool -> Bool
False =  λ(Bool : *) -> λ(True : Bool) -> λ(False : Bool) -> False
```

### Nat

``` haskell
> inductive Nat = (Zero : Nat) (Succ : Nat -> Nat)
Nat :: *
Nat =  ∀(Nat : *) -> Nat -> (Nat -> Nat) -> Nat
Zero :: ∀(Nat : *) -> Nat -> (Nat -> Nat) -> Nat
Zero =  λ(Nat : *) -> λ(Zero : Nat) -> λ(Succ : Nat -> Nat) -> Zero
Succ :: (∀(Nat : *) -> Nat -> (Nat -> Nat) -> Nat) -> ∀(Nat : *) -> Nat -> (Nat -> Nat) -> Nat
Succ =  λ(a : ∀(Nat : *) -> Nat -> (Nat -> Nat) -> Nat) -> λ(Nat : *) -> λ(Zero : Nat) -> λ(Succ : Nat -> Nat) -> Succ (a Nat Zero Succ)
```

### Maybe

``` haskell
> inductive Maybe (a : *) = (Nothing : Maybe a) (Just : a -> Maybe a)
Maybe :: * -> *
Maybe =  λ(a : *) -> ∀(Maybe : * -> *) -> Maybe a -> (a -> Maybe a) -> Maybe a
Nothing :: ∀(a : *) -> ∀(Maybe : * -> *) -> Maybe a -> (a -> Maybe a) -> Maybe a
Nothing =  λ(a : *) -> λ(Maybe : * -> *) -> λ(Nothing : Maybe a) -> λ(Just : a -> Maybe a) -> Nothing
Just :: ∀(a : *) -> a -> ∀(Maybe : * -> *) -> Maybe a -> (a -> Maybe a) -> Maybe a
Just =  λ(a : *) -> λ(b : a) -> λ(Maybe : * -> *) -> λ(Nothing : Maybe a) -> λ(Just : a -> Maybe a) -> Just b
```

### Eq

``` haskell
> inductive Eq (A : *) (a : A) (b : A) = (Refl : forall (x : A) -> Eq A x x)
Eq :: ∀(A : *) -> A -> A -> *
Eq =  λ(A : *) -> λ(a : A) -> λ(b : A) -> ∀(Eq : ∀(A : *) -> A -> A -> *) -> (∀(x : A) -> Eq A x x) -> Eq A a b
Refl :: ∀(A : *) -> ∀(x : A) -> ∀(Eq : ∀(A : *) -> A -> A -> *) -> (∀(x : A) -> Eq A x x) -> Eq A x x
Refl =  λ(A : *) -> λ(x : A) -> λ(Eq : ∀(A : *) -> A -> A -> *) -> λ(Refl : ∀(x : A) -> Eq A x x) -> Refl x
```

Examples
--------

You can also write any type or function in pure CoC:

### Booleans

```haskell
> :let Bool  = forall (a : *) -> a -> a -> a
Bool :: *
Bool =  ∀(a : *) -> a -> a -> a
> :let true  = \(a : *) -> \(t : a) -> \(_ : a) -> t
true :: ∀(a : *) -> a -> a -> a
true =  λ(a : *) -> λ(t : a) -> λ(_ : a) -> t
> :let false = \(a : *) -> \(_ : a) -> \(f : a) -> f
false :: ∀(a : *) -> a -> a -> a
false =  λ(a : *) -> λ(_ : a) -> λ(f : a) -> f
> :let if    = \(b : Bool) -> b
if :: (∀(a : *) -> a -> a -> a) -> ∀(a : *) -> a -> a -> a
if =  λ(b : ∀(a : *) -> a -> a -> a) -> b
> :let not   = \(b : Bool) -> b Bool false true
not :: (∀(a : *) -> a -> a -> a) -> ∀(a : *) -> a -> a -> a
not =  λ(b : ∀(a : *) -> a -> a -> a) -> b (∀(a : *) -> a -> a -> a) (λ(a : *) -> λ(_ : a) -> λ(f : a) -> f) (λ(a : *) -> λ(t : a) -> λ(_ : a) -> t)
> :let and   = \(a : Bool) -> \(b : Bool) -> a Bool b false
and :: (∀(a : *) -> a -> a -> a) -> (∀(a : *) -> a -> a -> a) -> ∀(a : *) -> a -> a -> a
and =  λ(a : ∀(a : *) -> a -> a -> a) -> λ(b : ∀(a : *) -> a -> a -> a) -> a (∀(a : *) -> a -> a -> a) b (λ(a : *) -> λ(_ : a) -> λ(f : a) -> f)
> :let or    = \(a : Bool) -> \(b : Bool) -> a Bool true b
or :: (∀(a : *) -> a -> a -> a) -> (∀(a : *) -> a -> a -> a) -> ∀(a : *) -> a -> a -> a
or =  λ(a : ∀(a : *) -> a -> a -> a) -> a (∀(a : *) -> a -> a -> a) (λ(a : *) -> λ(t : a) -> λ(_ : a) -> t)
```

Now we can play around it.

**Not**:
```haskell
> not true
_ :: ∀(a : *) -> a -> a -> a                -- Bool
_ =  λ(a : *) -> λ(_ : a) -> λ(f : a) -> f  -- false
```

**And**:
```haskell
> and true false
_ :: ∀(a : *) -> a -> a -> a                -- Bool
_ =  λ(a : *) -> λ(_ : a) -> λ(f : a) -> f  -- false
> and true true
_ :: ∀(a : *) -> a -> a -> a                -- Bool
_ =  λ(a : *) -> λ(t : a) -> λ(_ : a) -> t  -- true
```

**Or**:
```haskell
> or false true
_ :: ∀(a : *) -> a -> a -> a                -- Bool
_ =  λ(a : *) -> λ(t : a) -> λ(_ : a) -> t  -- true
> or false false
_ :: ∀(a : *) -> a -> a -> a                -- Bool
_ =  λ(a : *) -> λ(_ : a) -> λ(f : a) -> f  -- false
```

### Lists

```haskell
> :let List  = \(a : *) -> forall (list : *) -> (a -> list -> list) -> list -> list
List :: * -> *
List =  λ(a : *) -> ∀(list : *) -> (a -> list -> list) -> list -> list
> :let cons  = \(a : *) -> \(x : a) -> \(xs : List a) -> \(list : *) -> \(Cons : a -> list -> list) -> \(Nil : list) -> Cons x (xs list Cons Nil)
cons :: ∀(a : *) -> a -> λ(a : *) -> ∀(list : *) -> (a -> list -> list) -> list -> list a -> ∀(list : *) -> (a -> list -> list) -> list -> list
cons =  λ(a : *) -> λ(x : a) -> λ(xs : ∀(list : *) -> (a -> list -> list) -> list -> list) -> λ(list : *) -> λ(Cons : a -> list -> list) -> λ(Nil : list) -> Cons x (xs list Cons Nil)
> :let nil   = \(a : *) -> \(list : *) -> \(Cons : a -> list -> list) -> \(Nil : list) -> Nil
nil :: ∀(a : *) -> ∀(list : *) -> (a -> list -> list) -> list -> list
nil =  λ(a : *) -> λ(list : *) -> λ(Cons : a -> list -> list) -> λ(Nil : list) -> Nil
```

And now:
```haskell
> cons Bool true (cons Bool false (nil Bool))
_ :: ∀(list : *) -> ((∀(a : *) -> a -> a -> a) -> list -> list) -> list -> list -- List Bool
_ =  λ(list : *) -> λ(Cons : (∀(a : *) -> a -> a -> a) -> list -> list) -> λ(Nil : list) -> Cons (λ(a : *) -> λ(t : a) -> λ(_ : a) -> t) (Cons (λ(a : *) -> λ(_ : a) -> λ(f : a) -> f) Nil)
```
