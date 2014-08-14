# Module Documentation

## Module Data.Const

### Types

    newtype Const a b where
      Const :: a -> Const a b


### Type Class Instances

    instance applicativeConst :: (Monoid a) => Applicative (Const a)

    instance applyConst :: (Semigroup a) => Apply (Const a)

    instance bindConst :: (Semigroup a) => Bind (Const a)

    instance contravariantConst :: Contravariant (Const a)

    instance eqConst :: (Eq a) => Eq (Const a b)

    instance functorConst :: Functor (Const a)

    instance monoidConst :: (Monoid a) => Monoid (Const a b)

    instance ordConst :: (Ord a) => Ord (Const a b)

    instance semigroupConst :: (Semigroup a) => Semigroup (Const a b)

    instance semigroupoidConst :: Semigroupoid Const

    instance showConst :: (Show a) => Show (Const a b)


### Values

    getConst :: forall a b. Const a b -> a



