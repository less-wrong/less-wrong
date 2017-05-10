less-wrong
==========

I hope one day a nice prover with tactics would be here, but not it is just a simple CoC calulator.

Examples
--------

### Booleans

```
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
```
> not true
_ :: ∀(a : *) -> a -> a -> a                -- Bool
_ =  λ(a : *) -> λ(_ : a) -> λ(f : a) -> f  -- false
```

**And**:
```
> and true false
_ :: ∀(a : *) -> a -> a -> a                -- Bool
_ =  λ(a : *) -> λ(_ : a) -> λ(f : a) -> f  -- false
> and true true
_ :: ∀(a : *) -> a -> a -> a                -- Bool
_ =  λ(a : *) -> λ(t : a) -> λ(_ : a) -> t  -- true
```

**Or**:
```
> or false true
_ :: ∀(a : *) -> a -> a -> a                -- Bool
_ =  λ(a : *) -> λ(t : a) -> λ(_ : a) -> t  -- true
> or false false
_ :: ∀(a : *) -> a -> a -> a                -- Bool
_ =  λ(a : *) -> λ(_ : a) -> λ(f : a) -> f  -- false
```

### Lists

```
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
```
> cons Bool true (cons Bool false (nil Bool))
_ :: ∀(list : *) -> ((∀(a : *) -> a -> a -> a) -> list -> list) -> list -> list -- List Bool
_ =  λ(list : *) -> λ(Cons : (∀(a : *) -> a -> a -> a) -> list -> list) -> λ(Nil : list) -> Cons (λ(a : *) -> λ(t : a) -> λ(_ : a) -> t) (Cons (λ(a : *) -> λ(_ : a) -> λ(f : a) -> f) Nil)
```
