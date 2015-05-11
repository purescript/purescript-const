## Module Data.Const

This module defines the `Const` type constructor.

#### `Const`

``` purescript
newtype Const a b
  = Const a
```

##### Instances
``` purescript
instance eqConst :: (Eq a) => Eq (Const a b)
instance ordConst :: (Ord a) => Ord (Const a b)
instance boundedConst :: (Bounded a) => Bounded (Const a b)
instance showConst :: (Show a) => Show (Const a b)
instance semigroupoidConst :: Semigroupoid Const
instance semigroupConst :: (Semigroup a) => Semigroup (Const a b)
instance monoidConst :: (Monoid a) => Monoid (Const a b)
instance functorConst :: Functor (Const a)
instance invariantConst :: Invariant (Const a)
instance applyConst :: (Semigroup a) => Apply (Const a)
instance bindConst :: (Semigroup a) => Bind (Const a)
instance applicativeConst :: (Monoid a) => Applicative (Const a)
instance contravariantConst :: Contravariant (Const a)
instance foldableConst :: Foldable (Const a)
instance traversableConst :: Traversable (Const a)
```

The `Const` type constructor, which wraps its first type argument
and ignores its second. That is, `Const a b` is isomorphic to `a`
for any `b`.

`Const` has some useful instances. For example, the `Applicative`
instance allows us to collect results using a `Monoid` while
ignoring return values.

#### `getConst`

``` purescript
getConst :: forall a b. Const a b -> a
```

Unwrap a value of type `Const a b`.


