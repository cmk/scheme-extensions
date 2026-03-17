{-# LANGUAGE RankNTypes #-}

-- | Recursion schemes and streaming metamorphisms for 'Mu'.
--
-- Extends <https://hackage.haskell.org/package/data-fix data-fix>
-- with paramorphisms, zygomorphisms, Elgot algebras, mutual
-- recursion, pre\/postpromorphisms, and generic streaming
-- metamorphisms (Gibbons).
--
-- No @Recursive@\/@Corecursive@ typeclasses — all functions
-- operate directly on 'Mu'.
module Data.Functor.Foldable (
    -- * Folding
    fold,
    foldWithContext,
    foldWithAux,
    foldGen,
    foldM,

    -- * Unfolding
    unfold,
    unfoldShort,
    unfoldGen,

    -- * Refold
    refold,
    refoldGen,
    refoldM,

    -- * Elgot algebras
    elgot,
    coelgot,

    -- * Mutual recursion
    mutu,
    comutu,

    -- * Natural transformations
    comap,
    contramap,
    prepro,
    postpro,
    transverse,
    cotransverse,

    -- * Streaming metamorphisms (Gibbons)
    stream,
    astream,
    gstream,
) where

import Data.Bifunctor (Bifunctor (..))
import Data.Functor.Fixed
import qualified Data.Fix as F
import Control.Monad ((<=<))

---------------------------------------------------------------------
-- Folding
---------------------------------------------------------------------

-- | Fold a structure one layer at a time (catamorphism).
--
-- O(1) dispatch — the fold /is/ the representation.
{-# INLINE fold #-}
fold :: Algebra f a -> Mu f -> a
fold = F.foldMu

-- | Fold with access to the original subtree at each position
-- (paramorphism).
foldWithContext :: Functor f => (f (Mu f, a) -> a) -> Mu f -> a
foldWithContext alg = snd . fold (\f -> (wrap (fmap fst f), alg f))

-- | Fold with an auxiliary fold running in parallel (zygomorphism).
foldWithAux :: Functor f => Algebra f b -> (f (b, a) -> a) -> Mu f -> a
foldWithAux aux alg = snd . fold (\f -> (aux (fmap fst f), alg f))

-- | Generalized fold using a distributive law (gcata).
foldGen :: Functor f => Distribute f (Pair c) -> GAlgebra (Pair c) f b -> Mu f -> b
foldGen k phi = pairSnd . fold (lowerAlgebra k phi)

-- | Monadic fold (catamorphism with effects).
foldM :: (Traversable f, Monad m) => AlgebraM m f a -> Mu f -> m a
foldM alg = fold (alg <=< sequenceA)

---------------------------------------------------------------------
-- Unfolding
---------------------------------------------------------------------

-- | Build a structure from a seed one layer at a time (anamorphism).
unfold :: Functor f => Coalgebra f a -> a -> Mu f
unfold = F.unfoldMu

-- | Build from a seed with early termination (apomorphism).
unfoldShort :: Functor f => (a -> f (Either (Mu f) a)) -> a -> Mu f
unfoldShort coalg = go where go = wrap . fmap (either id go) . coalg

-- | Generalized unfold using a distributive law (gana).
unfoldGen
    :: (Functor f, Functor n, Monad n)
    => Distribute n f
    -> GCoalgebra n f b
    -> b -> Mu f
unfoldGen k psi = unfold (lowerCoalgebra k psi) . pure

---------------------------------------------------------------------
-- Refold
---------------------------------------------------------------------

-- | Unfold then fold, fused — no intermediate structure (hylomorphism).
{-# INLINE refold #-}
refold :: Functor f => Algebra f b -> Coalgebra f a -> a -> b
refold alg coalg = go where go = alg . fmap go . coalg

-- | Generalized hylomorphism using distributive laws on both sides.
refoldGen
    :: (Functor f, Functor n, Monad n)
    => Distribute f (Pair c)
    -> Distribute n f
    -> GAlgebra (Pair c) f b
    -> GCoalgebra n f r
    -> r -> b
refoldGen w m phi psi = pairSnd . refold (lowerAlgebra w phi) (lowerCoalgebra m psi) . pure

-- | Monadic hylomorphism: unfold then fold with effects, fused.
refoldM :: (Traversable f, Monad m) => AlgebraM m f b -> CoalgebraM m f a -> a -> m b
refoldM alg coalg = go
  where
    go a = do
        fa <- coalg a
        fb <- traverse go fa
        alg fb

---------------------------------------------------------------------
-- Elgot algebras
---------------------------------------------------------------------

-- | Elgot algebra: unfold with short-circuit.
elgot :: Functor f => Algebra f b -> (r -> Either b (f r)) -> r -> b
elgot phi psi = go
  where
    go r = case psi r of
        Left b -> b
        Right c -> phi (fmap go c)

-- | Dual of 'elgot': the algebra receives the original seed
-- alongside the recursively-folded structure.
coelgot :: Functor f => ((r, f b) -> b) -> Coalgebra f r -> r -> b
coelgot phi psi = go
  where
    go r = phi (r, fmap go (psi r))

---------------------------------------------------------------------
-- Mutual recursion
---------------------------------------------------------------------

-- | Mutual recursion: two algebras running simultaneously.
mutu :: Functor f => (f (Pair c b) -> b) -> (f (Pair b c) -> c) -> Mu f -> c
mutu phi' phi = pairSnd . fold (\fa ->
    let p = diagonal fa
     in bimap (phi' . fmap swapPair) phi p)

-- | Mutual corecursion: two coalgebras running simultaneously.
comutu :: Functor f => (b -> f (Either r b)) -> (r -> f (Either b r)) -> r -> Mu f
comutu psi' psi = unfold (fromEither . bimap (fmap swapEither . psi') psi) . Right
  where
    swapEither (Left x) = Right x
    swapEither (Right y) = Left y

---------------------------------------------------------------------
-- Natural transformations
---------------------------------------------------------------------

-- | Map over the first type parameter of a 'Bifunctor' base functor
-- via anamorphism.
comap :: (Bifunctor f, Functor (f a), Functor (f b)) => (a -> b) -> Mu (f a) -> Mu (f b)
comap g = unfold (first g . unwrap)

-- | Map via catamorphism. Semantically identical to 'comap'.
contramap :: (Bifunctor f, Functor (f a), Functor (f b)) => (a -> b) -> Mu (f a) -> Mu (f b)
contramap g = fold (wrap . first g)

-- | Fokkinga's prepromorphism.
prepro :: Functor f => (forall a. f a -> f a) -> Algebra f c -> Mu f -> c
prepro e alg = go where go = alg . fmap (go . hoistMu e) . unwrap

-- | Fokkinga's postpromorphism.
postpro :: Functor f => (forall a. f a -> f a) -> Coalgebra f r -> r -> Mu f
postpro e coalg = go where go = wrap . fmap (hoistMu e . go) . coalg

-- | Effectful 'hoistMu': sequence effects while transforming layers.
transverse :: (Functor f, Functor g) => (forall a. f (g a) -> g (f a)) -> Mu f -> g (Mu f)
transverse n = fold (fmap wrap . n)

-- | Coeffectful 'hoistMu': transform layers while distributing a functor.
cotransverse :: (Functor f, Functor g) => (forall a. g (f a) -> f (g a)) -> g (Mu f) -> Mu f
cotransverse n = unfold (n . fmap unwrap)

---------------------------------------------------------------------
-- Streaming metamorphisms (Gibbons)
--
-- Generic over the base functor. Input functor g, output functor f.
---------------------------------------------------------------------

-- | Core streaming metamorphism engine.
stream
    :: Functor f
    => (i -> g i)                      -- ^ project input
    -> (f o -> o)                      -- ^ embed output
    -> (state -> Maybe (f state))      -- ^ @process@: try to produce
    -> (state -> ((state -> state) -> i -> o) -> g i -> o)
                                       -- ^ @accum@: consume next input
    -> state -> i -> o
stream proj emb process accum = go
  where
    go state input =
        maybe
            (accum state (\f -> go (f state)) (proj input))
            (emb . fmap (`go` input))
            $ process state

-- | Streaming anamorphism: accumulator always consumes.
astream
    :: Functor f
    => (i -> g i)
    -> (f o -> o)
    -> (state -> Maybe (f state))
    -> (g i -> Pair (state -> state) i)
    -> state -> i -> o
astream proj emb process accum = stream proj emb process $
    \_state cont -> uncurryPair cont . accum

-- | Streaming generalized apomorphism: flush when input exhausted.
gstream
    :: Functor f
    => (i -> g i)
    -> (f o -> o)
    -> (state -> f state)              -- ^ @flush@
    -> (state -> Maybe (f state))      -- ^ @process@
    -> (g i -> Maybe (Pair (state -> state) i))
    -> state -> i -> o
gstream proj emb flush process accum = stream proj emb process $
    \state cont -> maybe (drain emb flush state) (uncurryPair cont) . accum
  where
    drain e coalg = go where go = e . fmap go . coalg
