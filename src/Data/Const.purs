-- | This module defines the `Const` type constructor.

module Data.Const where

import Data.Bifoldable (Bifoldable)
import Data.Foldable (Foldable)
import Data.Functor.Contravariant (Contravariant)
import Data.Functor.Invariant (Invariant, imapF)
import Data.Monoid (Monoid, mempty)
import Data.Traversable (Traversable)

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

instance eqConst :: (Eq a) => Eq (Const a b) where
  eq (Const x) (Const y) = x == y

instance ordConst :: (Ord a) => Ord (Const a b) where
  compare (Const x) (Const y) = compare x y

instance boundedConst :: (Bounded a) => Bounded (Const a b) where
  top = Const top
  bottom = Const bottom

instance showConst :: (Show a) => Show (Const a b) where
  show (Const x) = "Const (" ++ show x ++ ")"

instance semigroupoidConst :: Semigroupoid Const where
  compose _ (Const x) = Const x

instance semigroupConst :: (Semigroup a) => Semigroup (Const a b) where
  append (Const x) (Const y) = Const (x <> y)

instance monoidConst :: (Monoid a) => Monoid (Const a b) where
  mempty = Const mempty

instance functorConst :: Functor (Const a) where
  map _ (Const x) = Const x

instance invariantConst :: Invariant (Const a) where
  imap = imapF

instance applyConst :: (Semigroup a) => Apply (Const a) where
  apply (Const x) (Const y) = Const (x <> y)

instance bindConst :: (Semigroup a) => Bind (Const a) where
  bind (Const x) _ = Const x

instance applicativeConst :: (Monoid a) => Applicative (Const a) where
  pure _ = Const mempty

instance contravariantConst :: Contravariant (Const a) where
  cmap _ (Const x) = Const x

instance foldableConst :: Foldable (Const a) where
  foldr _ z _ = z
  foldl _ z _ = z
  foldMap _ _ = mempty

instance traversableConst :: Traversable (Const a) where
  traverse _ (Const x) = pure (Const x)
  sequence (Const x) = pure (Const x)
