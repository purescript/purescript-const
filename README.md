# Module Documentation

## Module Data.Const

#### `Const`

``` purescript
newtype Const a b
  = Const a
```


#### `getConst`

``` purescript
getConst :: forall a b. Const a b -> a
```


#### `eqConst`

``` purescript
instance eqConst :: (Eq a) => Eq (Const a b)
```


#### `ordConst`

``` purescript
instance ordConst :: (Ord a) => Ord (Const a b)
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