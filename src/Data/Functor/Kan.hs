{-# LANGUAGE RankNTypes #-}

-- | Kan extension connections for 'Fmt' and 'Fix'.
--
-- This module makes explicit the categorical structure underlying
-- the library:
--
-- * __Day convolution__: @(%)@ is Day convolution of @(->) m@.
--   'fold2' enables parallel folds over two structures.
--   'equalDay'/'compareDay' are structural equality/ordering.
--
-- * __Yoneda__: fuses chains of 'Data.Fmt.Fixed.hoist' into a
--   single traversal. @forall a. Fmt m a a ≅ m@ is the Yoneda
--   lemma.
--
-- * __Codensity__: right-associates monadic binds for efficient
--   tree building. @Codensity ((->) m) ≅ State m@.
--
-- * __Ran/Lan__: right/left Kan extensions. @Ran u Identity a ≅
--   (Rep u, a)@ for Representable @u@ — the foundation for
--   indexed cotraversals in profunctor-optics-strings.
--
-- * __Density__: comonad from @Lan f f@ (dual of Codensity).
--
-- * __Curried__: right adjoint to Day (@Day f -| Curried f@).
--   This is what makes 'Fmt' m's 'Data.Profunctor.Closed' instance
--   work.
module Data.Functor.Kan (
    -- * Day convolution
    equalDay,
    compareDay,
    recursiveEq,
    recursiveOrd,

    -- * Yoneda (hoist fusion)
    YonedaFix (..),
    liftYonedaFix,
    mapYonedaFix,
    lowerYonedaFix,

    -- * Codensity
    foldMCodensity,
    codensityToState,
    stateToCodensity,
    codensityToStateT,
    stateTToCodensity,
    -- codensityToComposedRep and composedRepToCodensity
    -- are re-exported via module Control.Monad.Codensity

    -- * Re-exports
    module Data.Functor.Day,
    module Data.Functor.Yoneda,
    module Control.Monad.Codensity,
    module Data.Functor.Kan.Ran,
    module Data.Functor.Kan.Lan,
    module Control.Comonad.Density,
    module Data.Functor.Day.Curried,
) where

import Control.Comonad.Density
import Control.Monad.Codensity hiding (improve)
import Control.Monad.Trans.State.Strict (StateT (..))
import Data.Foldable (toList)
import Data.Functor.Pattern (Cons (..))
import Data.Functor.Compose (Compose (..))
import Data.Functor.Fixed
import Data.Functor.Foldable

import Data.Functor.Classes (Eq1 (..), Ord1 (..))
import Data.Functor.Day
import Data.Functor.Day.Curried
import Data.Functor.Kan.Lan
import Data.Functor.Kan.Ran
import Data.Functor.Yoneda

---------------------------------------------------------------------
-- Day convolution
---------------------------------------------------------------------

-- | Structural equality via Day convolution.
--
-- Pairs up two @f@-layers element-by-element and checks that:
-- 1. The shapes match (via @eqF@)
-- 2. All paired elements satisfy the combining function
--
-- This is 'liftEq' decomposed into its Day components:
-- @Day f f Bool@ is "two layers paired with a boolean combiner".
--
-- >>> import Data.Functor.Kan
-- >>> import Data.Functor.Pattern
-- >>> import Data.Functor.Classes
-- >>> equalDay (liftEq (==)) (Day (Cons 1 True) (Cons 1 True) (&&))
-- True
-- >>> import Data.Functor.Classes
-- >>> equalDay (liftEq (==)) (Day (Cons 1 True) (Cons 2 True) (&&))
-- False
--
-- __Connection:__ @Eq1 f@ is exactly the structure needed to
-- evaluate @Day f f Bool@ — it pairs elements and combines
-- with a boolean function. This is Day convolution specialized
-- to the @Bool@ monoid under @(&&)@.
equalDay :: (Foldable f, Eq1 f) => Day f f Bool -> Bool
equalDay (Day f1 f2 fn) =
    liftEq (\_ _ -> True) f1 f2
        && and (zipWith fn (toList f1) (toList f2))

-- | Structural ordering via Day convolution.
--
-- Like 'equalDay' but produces an 'Ordering'.
--
-- >>> import Data.Functor.Kan
-- >>> import Data.Functor.Pattern
-- >>> compareDay (Day (Cons 1 EQ) (Cons 2 EQ) (<>))
-- LT
--
-- __Connection:__ @Ord1 f@ provides the Day structure for
-- @Ordering@ under @(<>)@.
compareDay :: (Foldable f, Ord1 f) => Day f f Ordering -> Ordering
compareDay (Day f1 f2 fn) =
    liftCompare (\_ _ -> EQ) f1 f2
        <> mconcat (zipWith fn (toList f1) (toList f2))

-- | Recursive equality via 'fold2' and 'equalDay'.
--
-- Compares two @Mu f@ values layer by layer using Day
-- convolution. Equivalent to @(==)@ from the @Eq (Mu f)@
-- instance, but expressed explicitly via Day.
--
-- >>> import Data.Functor.Kan
-- >>> import Data.Functor.Pattern
-- >>> recursiveEq (fromList [1,2,3 :: Int]) (fromList [1,2,3])
-- True
-- >>> recursiveEq (fromList [1,2,3 :: Int]) (fromList [1,2,4])
-- False
--
-- __Connection:__ this is @fold2 (equalDay (liftEq (==)))@ —
-- the Eq instance decomposed into its Day + fold components.
recursiveEq :: (Functor f, Foldable f, Eq1 f) => Mu f -> Mu f -> Bool
recursiveEq x y = equalDay (Day (unwrap x) (unwrap y) (\a b -> recursiveEq a b))

-- | Recursive ordering via Day convolution.
--
-- >>> import Data.Functor.Kan
-- >>> import Data.Functor.Pattern
-- >>> recursiveOrd (fromList [1,2,3 :: Int]) (fromList [1,2,4])
-- LT
recursiveOrd :: (Functor f, Foldable f, Ord1 f) => Mu f -> Mu f -> Ordering
recursiveOrd x y = compareDay (Day (unwrap x) (unwrap y) (\a b -> recursiveOrd a b))

---------------------------------------------------------------------
-- Yoneda
---------------------------------------------------------------------

-- | Yoneda-encoded natural transformation accumulator for 'Fix'.
--
-- @YonedaFix f g@ holds a @Mu f@ together with a pending
-- natural transformation @f ~> g@. Multiple transformations
-- compose via 'mapYonedaFix' in O(1) (function composition).
-- The actual traversal happens once at 'lowerYonedaFix'.
--
-- Without Yoneda:
--
-- @
-- hoist n3 . hoist n2 . hoist n1   -- 3 full traversals
-- @
--
-- With Yoneda:
--
-- @
-- lowerYonedaFix . mapYonedaFix n3 . mapYonedaFix n2 . mapYonedaFix n1 . liftYonedaFix
-- -- 1 traversal
-- @
--
-- __Connection:__ this is the Yoneda lemma for the functor
-- category @[Hask, Hask]@: natural transformations out of
-- @f@ are equivalent to @f@ itself. Accumulating them as
-- function composition defers the cost.
data YonedaFix f g = YonedaFix (forall a. f a -> g a) (Mu f)

-- | Lift a 'Fix' into 'YonedaFix' with the identity transformation.
--
-- >>> import Data.Functor.Kan
-- >>> import Data.Functor.Pattern
-- >>> let xs = fromList [1, 2, 3 :: Int]
-- >>> toList (lowerYonedaFix (liftYonedaFix xs))
-- [1,2,3]
liftYonedaFix :: Mu f -> YonedaFix f f
liftYonedaFix = YonedaFix id

-- | Apply a natural transformation in O(1) (just composition).
--
-- >>> import Data.Functor.Kan
-- >>> import Data.Functor.Pattern
-- >>> let xs = fromList [1, 2, 3 :: Int]
-- >>> let inc Nil = Nil; inc (Cons a r) = Cons (a + 1) r
-- >>> toList (lowerYonedaFix (mapYonedaFix inc (liftYonedaFix xs)))
-- [2,3,4]
mapYonedaFix :: (forall a. g a -> h a) -> YonedaFix f g -> YonedaFix f h
mapYonedaFix n (YonedaFix m t) = YonedaFix (n . m) t

-- | Lower the accumulated transformation, performing a single traversal.
--
-- >>> import Data.Functor.Kan
-- >>> import Data.Functor.Pattern
-- >>> let xs = fromList [1, 2, 3 :: Int]
-- >>> let inc Nil = Nil; inc (Cons a r) = Cons (a + 1) r
-- >>> toList (lowerYonedaFix (mapYonedaFix inc (mapYonedaFix inc (liftYonedaFix xs))))
-- [3,4,5]
lowerYonedaFix :: YonedaFix f g -> Mu g
lowerYonedaFix (YonedaFix n t) = hoistMu n t

---------------------------------------------------------------------
-- Codensity
---------------------------------------------------------------------

-- | Monadic fold accelerated by 'Codensity'.
--
-- Standard 'Data.Fmt.Fixed.foldM' can be O(n^2) on deep
-- left-nested trees because @(>>=)@ associates to the left.
-- 'Codensity' right-associates the binds:
--
-- @
-- foldMCodensity alg = lowerCodensity . foldM (Codensity . alg)
-- @
--
-- >>> import Data.Functor.Kan
-- >>> import Data.Functor.Pattern
-- >>> let xs = fromList [1, 2, 3 :: Int]
-- >>> foldMCodensity (\case Nil -> pure 0; Cons a b -> pure (a + b)) xs :: Maybe Int
-- Just 6
--
-- __Connection:__ @Codensity m a = forall b. (a -> m b) -> m b@.
-- This is the CPS transform of @m@ — it right-associates binds,
-- turning O(n^2) left-nested @>>=@ into O(n).
foldMCodensity
    :: (Traversable f, Monad m)
    => AlgebraM m f a
    -> Mu f
    -> m a
foldMCodensity alg = lowerCodensity . fold go
  where
    go fa = Codensity $ \k -> do
        a <- alg =<< traverse (\(Codensity c) -> c pure) fa
        k a

-- | Convert between @Codensity ((->) m)@ and @State m@.
--
-- @Codensity ((->) m) a = forall b. (a -> m -> b) -> m -> b@
--
-- This is isomorphic to @State m a = m -> (a, m)@ — both
-- are "computations that read and transform an @m@ environment."
--
-- >>> import Data.Functor.Kan
-- >>> codensityToState (Codensity (\k m -> k (m + 1) (m * 2))) (10 :: Int)
-- (11,20)
--
-- __Connection:__ @Codensity@ of a representable functor @(->) m@
-- gives the state monad for @m@. This is because
-- @Ran ((->) m) ((->) m) ≅ (->) m ∘ (->) m ≅ State m@.
codensityToState :: Codensity ((->) m) a -> m -> (a, m)
codensityToState (Codensity f) m = f (\a m' -> (a, m')) m

-- | Inverse of 'codensityToState'.
--
-- >>> import Data.Functor.Kan
-- >>> stateToCodensity (\m -> (m + 1, m * 2)) `codensityToState` (10 :: Int)
-- (11,20)
stateToCodensity :: (m -> (a, m)) -> Codensity ((->) m) a
stateToCodensity f = Codensity $ \k m -> let (a, m') = f m in k a m'

-- | Convert @Codensity (Compose ((->) m) n)@ to @StateT m n@.
--
-- The higher-kinded generalization of 'codensityToState':
--
-- @
-- Codensity ((->) m)             a  ≅  State  m   a
-- Codensity (Compose ((->) m) n) a  ≅  StateT m n a
-- @
--
-- Unwrapping:
--
-- @
-- Codensity (Compose ((->) m) n) a
--   = forall b. (a -> m -> n b) -> m -> n b
-- @
--
-- which is @StateT m n@ in CPS form.
--
-- >>> import Data.Functor.Kan
-- >>> import Control.Monad.Trans.State.Strict
-- >>> let c = Codensity $ \k m -> k (m + 1) (m * 2) :: Codensity (Compose ((->) Int) Identity) Int
-- >>> runStateT (codensityToStateT c) 10
-- Identity (11,20)
--
-- __Connection:__ for any 'Representable' @u@ with @Rep u = r@:
-- @Codensity (Compose u n) ≅ StateT r n@. With @u = (->) m@,
-- @Rep = m@, this gives @StateT m n@. With @u = ShortByteString@,
-- @Rep = Int@, you get @StateT Int n@ — an effectful indexed
-- traversal state machine.
codensityToStateT :: Monad n => Codensity (Compose ((->) m) n) a -> StateT m n a
codensityToStateT (Codensity f) = StateT $ \m ->
    getCompose (f (\a -> Compose (\m' -> pure (a, m')))) m

-- | Inverse of 'codensityToStateT'.
--
-- >>> import Data.Functor.Kan
-- >>> import Control.Monad.Trans.State.Strict
-- >>> codensityToState (stateTToCodensity (state (\m -> (m + 1, m * 2)))) (10 :: Int)
-- (11,20)
stateTToCodensity :: Monad n => StateT m n a -> Codensity (Compose ((->) m) n) a
stateTToCodensity (StateT st) = Codensity $ \k ->
    Compose $ \m -> st m >>= \(a, m') -> getCompose (k a) m'
