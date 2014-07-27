# Module Documentation

## Module Data.Const

### Types

    data Const a b where
      Const :: a -> Const a b


### Type Class Instances

    instance contravariantConst :: Contravariant (Const a)

    instance eqConst :: (Eq a) => Eq (Const a b)

    instance functorConst :: Functor (Const a)

    instance ordConst :: (Ord a) => Ord (Const a b)

    instance showConst :: (Show a) => Show (Const a b)


### Values

    getConst :: forall a b. Const a b -> a



