# Module Documentation

## Module Data.Const


This module defines the `Const` type constructor.

#### `Const`

``` purescript
newtype Const a b
  = Const a
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

#### `eqConst`

``` purescript
instance eqConst :: (Eq a) => Eq (Const a b)
```


#### `ordConst`

``` purescript
instance ordConst :: (Ord a) => Ord (Const a b)
```


#### `boundedConst`

``` purescript
instance boundedConst :: (Bounded a) => Bounded (Const a b)
```


#### `showConst`

``` purescript
instance showConst :: (Show a) => Show (Const a b)
```


#### `semigroupoidConst`

``` purescript
instance semigroupoidConst :: Semigroupoid Const
```


#### `semigroupConst`

``` purescript
instance semigroupConst :: (Semigroup a) => Semigroup (Const a b)
```


#### `monoidConst`

``` purescript
instance monoidConst :: (Monoid a) => Monoid (Const a b)
```


#### `functorConst`

``` purescript
instance functorConst :: Functor (Const a)
```


#### `applyConst`

``` purescript
instance applyConst :: (Semigroup a) => Apply (Const a)
```


#### `bindConst`

``` purescript
instance bindConst :: (Semigroup a) => Bind (Const a)
```


#### `applicativeConst`

``` purescript
instance applicativeConst :: (Monoid a) => Applicative (Const a)
```


#### `contravariantConst`

``` purescript
instance contravariantConst :: Contravariant (Const a)
```


#### `foldableConst`

``` purescript
instance foldableConst :: Foldable (Const a)
```


#### `traversableConst`

``` purescript
instance traversableConst :: Traversable (Const a)
```




