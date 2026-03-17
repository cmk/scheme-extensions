{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

-- | Fixed-point types, strict pair, algebra types, and distributive laws.
--
-- Re-exports 'Mu', 'Fix', and 'Nu' from
-- <https://hackage.haskell.org/package/data-fix data-fix>.
module Data.Functor.Fixed (
    -- * Fixed points (from data-fix)
    Mu (..),
    Fix (..),
    Nu (..),
    hoistMu,
    hoistFix,
    hoistNu,

    -- * Wrapping and unwrapping
    wrap,
    unwrap,

    -- * Strict pair
    Pair (..),
    pairFst,
    pairSnd,
    diagonal,
    swapPair,
    uncurryPair,
    fromEither,

    -- * Algebras
    Algebra,
    Coalgebra,
    GAlgebra,
    GCoalgebra,
    AlgebraM,
    CoalgebraM,

    -- * Distributive laws
    Distribute,
    lowerAlgebra,
    lowerCoalgebra,

    -- * Algebra combinators
    zipAlgebras,
) where

import Data.Bifoldable (Bifoldable (..))
import Data.Bifunctor (Bifunctor (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Fix (Fix (..), Mu (..), Nu (..), hoistFix, hoistMu, hoistNu)
import qualified Data.Fix as F
import Control.Monad (join)

---------------------------------------------------------------------
-- Wrapping and unwrapping
---------------------------------------------------------------------

-- | Inject one layer into the fixed point.
{-# INLINE wrap #-}
wrap :: Functor f => f (Mu f) -> Mu f
wrap = F.wrapMu

-- | Peel off one layer (Lambek's lemma).
unwrap :: Functor f => Mu f -> f (Mu f)
unwrap = F.unwrapMu

---------------------------------------------------------------------
-- Strict pair
---------------------------------------------------------------------

-- | Strict pair for streaming metamorphism accumulators
-- and generalized recursion schemes.
infix 2 :!:
data Pair a b = !a :!: !b
    deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

instance Bifunctor Pair where
    bimap f g (a :!: b) = f a :!: g b

instance Bifoldable Pair where
    bifoldMap f g (a :!: b) = f a `mappend` g b

instance Bitraversable Pair where
    bitraverse f g (a :!: b) = (:!:) <$> f a <*> g b

-- | First projection.
{-# INLINE pairFst #-}
pairFst :: Pair a b -> a
pairFst (a :!: _) = a

-- | Second projection.
{-# INLINE pairSnd #-}
pairSnd :: Pair a b -> b
pairSnd (_ :!: b) = b

-- | Strict diagonal: @x -> x :!: x@.
{-# INLINE diagonal #-}
diagonal :: a -> Pair a a
diagonal x = x :!: x

-- | Swap the components.
{-# INLINE swapPair #-}
swapPair :: Pair a b -> Pair b a
swapPair (a :!: b) = b :!: a

-- | Uncurry a function over a strict 'Pair'.
{-# INLINE uncurryPair #-}
uncurryPair :: (a -> b -> c) -> Pair a b -> c
uncurryPair f (a :!: b) = f a b

-- | Collapse an @Either a a@ to @a@.
{-# INLINE fromEither #-}
fromEither :: Either a a -> a
fromEither = either id id

---------------------------------------------------------------------
-- Algebras
---------------------------------------------------------------------

type Algebra f a = f a -> a

type Coalgebra f a = a -> f a

type GAlgebra w f a = f (w a) -> a

type GCoalgebra n f a = a -> f (n a)

type AlgebraM m f a = f a -> m a

type CoalgebraM m f a = a -> m (f a)

---------------------------------------------------------------------
-- Distributive laws
---------------------------------------------------------------------

-- | A natural transformation that commutes two functors.
type Distribute f g = forall a. f (g a) -> g (f a)

-- | Lower a generalized algebra to an ordinary algebra.
lowerAlgebra
    :: Functor f
    => Distribute f (Pair r)
    -> GAlgebra (Pair r) f b
    -> Algebra f (Pair r b)
lowerAlgebra k phi = fmap phi . k . fmap dup
  where
    dup p = pairFst p :!: p

-- | Lower a generalized coalgebra to an ordinary coalgebra.
lowerCoalgebra
    :: (Functor f, Functor n, Monad n)
    => Distribute n f
    -> GCoalgebra n f b
    -> n b -> f (n b)
lowerCoalgebra k psi = fmap join . k . fmap psi

---------------------------------------------------------------------
-- Algebra combinators
---------------------------------------------------------------------

-- | Run two algebras in parallel, collecting both results in a 'Pair'.
zipAlgebras :: Functor f => Algebra f c -> Algebra f d -> Algebra f (Pair c d)
zipAlgebras f g fa = f (fmap pairFst fa) :!: g (fmap pairSnd fa)
