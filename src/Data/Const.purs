module Data.Const where

import Control.Applicative (class Applicative, pure)
import Control.Apply (class Apply)
import Control.Bind (class Bind)
import Control.Semigroupoid (class Semigroupoid)

import Data.BooleanAlgebra (class BooleanAlgebra)
import Data.Bounded (class Bounded, bottom, top)
import Data.CommutativeRing (class CommutativeRing)
import Data.Eq (class Eq, (==))
import Data.EuclideanRing (class EuclideanRing, mod, degree, (/))
import Data.Field (class Field)
import Data.Foldable (class Foldable)
import Data.Functor (class Functor)
import Data.Functor.Contravariant (class Contravariant)
import Data.Functor.Invariant (class Invariant, imapF)
import Data.HeytingAlgebra (class HeytingAlgebra, not, implies, tt, ff, (&&), (||))
import Data.Monoid (class Monoid, mempty)
import Data.Ord (class Ord, compare)
import Data.Ring (class Ring, (-))
import Data.Semigroup (class Semigroup, (<>))
import Data.Semiring (class Semiring, one, zero, (+), (*))
import Data.Show (class Show, show)
import Data.Traversable (class Traversable)

-- | The `Const` type constructor, which wraps its first type argument
-- | and ignores its second. That is, `Const a b` is isomorphic to `a`
-- | for any `b`.
-- |
-- | `Const` has some useful instances. For example, the `Applicative`
-- | instance allows us to collect results using a `Monoid` while
-- | ignoring return values.
newtype Const a b = Const a

-- | Unwrap a value of type `Const a b`.
getConst :: forall a b. Const a b ->  a
getConst (Const x) = x

instance eqConst :: Eq a => Eq (Const a b) where
  eq (Const x) (Const y) = x == y

instance ordConst :: Ord a => Ord (Const a b) where
  compare (Const x) (Const y) = compare x y

instance boundedConst :: Bounded a => Bounded (Const a b) where
  top = Const top
  bottom = Const bottom

instance showConst :: Show a => Show (Const a b) where
  show (Const x) = "(Const " <> show x <> ")"

instance semigroupoidConst :: Semigroupoid Const where
  compose _ (Const x) = Const x

instance semigroupConst :: Semigroup a => Semigroup (Const a b) where
  append (Const x) (Const y) = Const (x <> y)

instance monoidConst :: Monoid a => Monoid (Const a b) where
  mempty = Const mempty

instance semiringConst :: Semiring a => Semiring (Const a b) where
  add (Const x) (Const y) = Const (x + y)
  zero = Const zero
  mul (Const x) (Const y) = Const (x * y)
  one = Const one

instance ringConst :: Ring a => Ring (Const a b) where
  sub (Const x) (Const y) = Const (x - y)

instance euclideanRingConst :: EuclideanRing a => EuclideanRing (Const a b) where
  degree (Const x) = degree x
  div (Const x) (Const y) = Const (x / y)
  mod (Const x) (Const y) = Const (x `mod` y)

instance commutativeRingConst :: CommutativeRing a => CommutativeRing (Const a b)

instance fieldConst :: Field a => Field (Const a b)

instance heytingAlgebraConst :: HeytingAlgebra a => HeytingAlgebra (Const a b) where
  ff = Const ff
  tt = Const tt
  implies (Const x) (Const y) = Const (x `implies` y)
  conj (Const x) (Const y) = Const (x && y)
  disj (Const x) (Const y) = Const (x || y)
  not (Const x) = Const (not x)

instance booleanAlgebraConst :: BooleanAlgebra a => BooleanAlgebra (Const a b)

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
