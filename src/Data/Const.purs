module Data.Const where

  import Data.Contravariant (Contravariant, (>$<))
  import Data.Monoid (Monoid, mempty)

  newtype Const a b = Const a

  getConst :: forall a b. Const a b ->  a
  getConst (Const x) = x

  instance eqConst :: (Eq a) => Eq (Const a b) where
    (==) (Const x) (Const y) = x == y

    (/=) c         c'        = not (c == c')

  instance showConst :: (Show a) => Show (Const a b) where
    show (Const x) = show x

  instance ordConst :: (Ord a) => Ord (Const a b) where
    compare (Const x) (Const y) = compare x y

  instance semigroupoidConst :: Semigroupoid Const where
    (<<<) _ (Const x) = Const x

  instance semigroupConst :: (Semigroup a) => Semigroup (Const a b) where
    (<>) (Const x) (Const y) = Const (x <> y)

  instance monoidConst :: (Monoid a) => Monoid (Const a b) where
    mempty = Const mempty

  instance functorConst :: Functor (Const a) where
    (<$>) _ (Const x) = Const x

  instance applyConst :: (Semigroup a) => Apply (Const a) where
    (<*>) (Const x) (Const y) = Const (x <> y)

  instance bindConst :: (Semigroup a) => Bind (Const a) where
    (>>=) (Const x) _ = Const x

  instance applicativeConst :: (Monoid a) => Applicative (Const a) where
    pure _ = Const mempty

  instance contravariantConst :: Contravariant (Const a) where
    (>$<) _ (Const x) = Const x
