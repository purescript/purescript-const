module Data.Const where

import Prelude

import Data.Foldable (class Foldable)
import Data.Functor.Contravariant (class Contravariant)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.Monoid (class Monoid, mempty)
import Data.Newtype (class Newtype)
import Data.Traversable (class Traversable)

-- | The `Const` type constructor, which wraps its first type argument
-- | and ignores its second. That is, `Const a b` is isomorphic to `a`
-- | for any `b`.
-- |
-- | `Const` has some useful instances. For example, the `Applicative`
-- | instance allows us to collect results using a `Monoid` while
-- | ignoring return values.
newtype Const a b = Const a

derive instance newtypeConst :: Newtype (Const a b) _

derive newtype instance eqConst :: Eq a => Eq (Const a b)

derive newtype instance ordConst :: Ord a => Ord (Const a b)

derive newtype instance boundedConst :: Bounded a => Bounded (Const a b)

instance showConst :: Show a => Show (Const a b) where
  show (Const x) = "(Const " <> show x <> ")"

instance semigroupoidConst :: Semigroupoid Const where
  compose _ (Const x) = Const x

derive newtype instance semigroupConst :: Semigroup a => Semigroup (Const a b)

derive newtype instance monoidConst :: Monoid a => Monoid (Const a b)

derive newtype instance semiringConst :: Semiring a => Semiring (Const a b)

derive newtype instance ringConst :: Ring a => Ring (Const a b)

derive newtype instance euclideanRingConst :: EuclideanRing a => EuclideanRing (Const a b)

derive newtype instance commutativeRingConst :: CommutativeRing a => CommutativeRing (Const a b)

derive newtype instance fieldConst :: Field a => Field (Const a b)

derive newtype instance heytingAlgebraConst :: HeytingAlgebra a => HeytingAlgebra (Const a b)

derive newtype instance booleanAlgebraConst :: BooleanAlgebra a => BooleanAlgebra (Const a b)

instance functorConst :: Functor (Const a) where
  map _ (Const x) = Const x

instance invariantConst :: Invariant (Const a) where
  imap = imapF

instance contravariantConst :: Contravariant (Const a) where
  cmap _ (Const x) = Const x

instance applyConst :: Semigroup a => Apply (Const a) where
  apply (Const x) (Const y) = Const (x <> y)

instance bindConst :: Semigroup a => Bind (Const a) where
  bind (Const x) _ = Const x

instance applicativeConst :: Monoid a => Applicative (Const a) where
  pure _ = Const mempty

instance foldableConst :: Foldable (Const a) where
  foldr _ z _ = z
  foldl _ z _ = z
  foldMap _ _ = mempty

instance traversableConst :: Traversable (Const a) where
  traverse _ (Const x) = pure (Const x)
  sequence (Const x) = pure (Const x)
