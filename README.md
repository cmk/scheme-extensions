[![CI](https://github.com/cmk/scheme-extensions/actions/workflows/ci.yml/badge.svg)](https://github.com/cmk/scheme-extensions/actions/workflows/ci.yml)

# scheme-extensions

Extended recursion schemes, streaming metamorphisms, and Kan
extension connections for
[data-fix](https://hackage.haskell.org/package/data-fix)'s
`Mu`/`Fix`/`Nu` types.

Complements
[recursion-schemes](https://hackage.haskell.org/package/recursion-schemes)
without the `Recursive`/`Corecursive` typeclass machinery —
all functions operate directly on `Mu`.

## Quick start

```haskell
import Data.Functor.Fixed
import Data.Functor.Foldable
import Data.Functor.Pattern

-- Build a list as Mu (Cons a)
>>> let xs = fromList [1, 2, 3 :: Int]

-- Fold (catamorphism)
>>> fold (\case Nil -> 0; Cons a b -> a + b) xs
6

-- Unfold (anamorphism)
>>> toList (unfold (\n -> if n > 5 then Nil else Cons n (n + 1)) 1)
[1,2,3,4,5]

-- Refold (hylomorphism) — no intermediate structure
>>> refold (\case Nil -> 0; Cons a b -> a + b)
          (\n -> if n > 5 then Nil else Cons n (n + 1))
          1
15
```

## Modules

### Data.Functor.Fixed

Re-exports `Mu`/`Fix`/`Nu` from data-fix, plus:

- **`Pair`** — strict pair (`!a :!: !b`) for accumulator-style schemes
- **Algebra types** — `Algebra`, `Coalgebra`, `GAlgebra`, `GCoalgebra`, `AlgebraM`, `CoalgebraM`
- **Distributive laws** — `Distribute`, `lowerAlgebra`, `lowerCoalgebra`
- **`wrap`/`unwrap`** — inject/project one layer of `Mu`
- **`zipAlgebras`** — run two algebras in parallel

### Data.Functor.Foldable

Recursion schemes on `Mu`, no typeclasses:

```haskell
-- Basic
fold            :: (f a -> a) -> Mu f -> a                    -- catamorphism
unfold          :: (a -> f a) -> a -> Mu f                    -- anamorphism
refold          :: (f b -> b) -> (a -> f a) -> a -> b          -- hylomorphism

-- Paramorphism (fold with access to original subtree)
foldWithContext  :: (f (Mu f, a) -> a) -> Mu f -> a

-- Zygomorphism (fold with auxiliary fold in parallel)
foldWithAux     :: (f b -> b) -> (f (b, a) -> a) -> Mu f -> a

-- Generalized (via distributive laws)
foldGen         :: Functor f => Distribute f (Pair c) -> GAlgebra (Pair c) f b -> Mu f -> b
unfoldGen       :: (Functor f, Functor n, Monad n) => Distribute n f -> GCoalgebra n f b -> b -> Mu f
refoldGen       :: (Functor f, Functor n, Monad n) => Distribute f (Pair c) -> Distribute n f -> GAlgebra (Pair c) f b -> GCoalgebra n f r -> r -> b

-- Monadic
foldM           :: (Traversable f, Monad m) => (f a -> m a) -> Mu f -> m a
refoldM         :: (Traversable f, Monad m) => (f b -> m b) -> (a -> m (f a)) -> a -> m b

-- Elgot algebras (unfold with short-circuit / fold with seed)
elgot           :: (f b -> b) -> (r -> Either b (f r)) -> r -> b
coelgot         :: ((r, f b) -> b) -> (r -> f r) -> r -> b

-- Mutual recursion
mutu            :: (f (Pair c b) -> b) -> (f (Pair b c) -> c) -> Mu f -> c
comutu          :: (b -> f (Either r b)) -> (r -> f (Either b r)) -> r -> Mu f

-- Natural transformations
comap           :: (Bifunctor f, Functor (f a), Functor (f b)) => (a -> b) -> Mu (f a) -> Mu (f b)
contramap       :: (Bifunctor f, Functor (f a), Functor (f b)) => (a -> b) -> Mu (f a) -> Mu (f b)
prepro          :: (forall a. f a -> f a) -> (f c -> c) -> Mu f -> c
postpro         :: (forall a. f a -> f a) -> (r -> f r) -> r -> Mu f
transverse      :: (forall a. f (g a) -> g (f a)) -> Mu f -> g (Mu f)
cotransverse    :: (forall a. g (f a) -> f (g a)) -> g (Mu f) -> Mu f
```

### Streaming metamorphisms (Gibbons)

#### Theory

A **metamorphism** is a fold followed by an unfold. The streaming
variants interleave accumulation (fold) and production (unfold)
so that output can be emitted before all input is consumed. This
avoids materializing the entire intermediate structure in memory.

The core engine is `stream`, which at each step tries to *produce*
output from the current state. If it can, it emits and continues.
If not, it *consumes* the next input element via the accumulator.

The functions are generic over both the base functor and the
fixed-point type — the caller passes project/embed functions:

```haskell
stream  :: Functor f => (i -> g i) -> (f o -> o) -> (state -> Maybe (f state)) -> (state -> ((state -> state) -> i -> o) -> g i -> o) -> state -> i -> o
astream :: Functor f => (i -> g i) -> (f o -> o) -> (state -> Maybe (f state)) -> (g i -> Pair (state -> state) i) -> state -> i -> o
gstream :: Functor f => (i -> g i) -> (f o -> o) -> (state -> f state) -> (state -> Maybe (f state)) -> (g i -> Maybe (Pair (state -> state) i)) -> state -> i -> o
```

#### Example 1: identity stream

```haskell
import Data.Functor.Fixed
import Data.Functor.Foldable
import Data.Functor.Pattern

-- Stream that accumulates all input, then flushes:
let flush = \case [] -> Nil; (a:as) -> Cons a as
    result = fstream unwrap wrap flush (\acc b -> acc ++ [b]) flush [] input
-- toList result == toList input
```

#### Example 2: element-wise transformation

```haskell
import Data.Functor.Fixed
import Data.Functor.Foldable
import Data.Functor.Pattern

-- Stream that transforms each element by (+1):
let flush = \case [] -> Nil; (a:as) -> Cons a as
    result = fstream unwrap wrap flush (\acc b -> acc ++ [b + 1]) flush [] (fromList [1,2,3])
-- toList result == [2,3,4]
```

### Data.Functor.Pattern

The `Cons` pattern functor for list-like structures:

```haskell
data Cons a b = Nil | Cons !a b
```

`Mu (Cons a)` is a Church-encoded list. `Nu (Cons a)` is a
lazy stream. Lazy `ByteString` and `Text` are `Mu (Cons chunk)`
internally.

```haskell
-- Cons-specialized streaming
fstream  :: (i -> Cons b i) -> (Cons a o -> o) -> (state -> Cons a state) -> (state -> b -> state) -> (state -> Cons a state) -> state -> i -> o

-- List conversion
toList   :: Mu (Cons a) -> [a]
fromList :: [a] -> Mu (Cons a)

-- Nu-based iteration (codata)
iterate  :: (a -> a) -> a -> Nu (Cons a)    -- infinite stream
repeat   :: a -> Nu (Cons a)                -- constant stream

-- Combinators
elim, toCons, fromCons, consFst, consSnd, isCons, isNil,
conses, filterNils, foldCons, gatherCons, partitionCons,
mapCons, consCurry, consUncurry, pairCons, unpairWith,
reassocLR, reassocRL, swapCons, showsPrecF

-- Distributive law specializations
distAna, distCata, distTuple, distEither, seqEither
```

### Data.Functor.Kan

Connections to
[kan-extensions](https://hackage.haskell.org/package/kan-extensions).

#### Day convolution (structural comparison)

##### Theory

`Day f g a = exists b c. (f b, g c, b -> c -> a)` is the
tensor product of two functors. When `f = g` and `a = Bool`,
a `Day f f Bool` pairs up corresponding elements from two
`f`-layers with a boolean combiner — exactly what structural
equality needs. This is `Eq1`/`Ord1` decomposed into its
Day components.

```haskell
equalDay     :: (Foldable f, Eq1 f)  => Day f f Bool -> Bool
compareDay   :: (Foldable f, Ord1 f) => Day f f Ordering -> Ordering
recursiveEq  :: (Functor f, Foldable f, Eq1 f)  => Mu f -> Mu f -> Bool
recursiveOrd :: (Functor f, Foldable f, Ord1 f) => Mu f -> Mu f -> Ordering
```

##### Example 1: comparing two lists

```haskell
import Data.Functor.Kan
import Data.Functor.Pattern

>>> recursiveEq (fromList [1,2,3 :: Int]) (fromList [1,2,3])
True
>>> recursiveEq (fromList [1,2,3 :: Int]) (fromList [1,2,4])
False
```

##### Example 2: ordering via Day

```haskell
import Data.Functor.Kan
import Data.Functor.Pattern

>>> recursiveOrd (fromList [1,2,3 :: Int]) (fromList [1,2,4])
LT
>>> recursiveOrd (fromList [1,2,3 :: Int]) (fromList [1,2,3])
EQ
```

#### Yoneda (hoist fusion)

##### Theory

Multiple `hoistMu` calls (natural transformations on `Mu`)
each traverse the entire structure. `YonedaFix` accumulates
transformations via O(1) function composition, paying the
traversal cost once at `lowerYonedaFix`. This is the Yoneda
lemma for the functor category `[Hask, Hask]`.

```haskell
liftYonedaFix  :: Mu f -> YonedaFix f f
mapYonedaFix   :: (forall a. g a -> h a) -> YonedaFix f g -> YonedaFix f h
lowerYonedaFix :: YonedaFix f g -> Mu g
```

##### Example 1: fusing two transformations

```haskell
import Data.Functor.Kan
import Data.Functor.Pattern
import Data.Functor.Fixed

let xs = fromList [1, 2, 3 :: Int]
    inc Nil = Nil; inc (Cons a r) = Cons (a + 1) r
    dbl Nil = Nil; dbl (Cons a r) = Cons (a * 2) r
-- Two hoists = 2 traversals:
    direct = hoistMu inc (hoistMu dbl xs)
-- Via Yoneda = 1 traversal:
    fused = lowerYonedaFix (mapYonedaFix inc (mapYonedaFix dbl (liftYonedaFix xs)))
-- toList direct == toList fused == [3,5,7]
```

##### Example 2: identity fusion

```haskell
import Data.Functor.Kan
import Data.Functor.Pattern

let xs = fromList [1, 2, 3 :: Int]
-- lowerYonedaFix . liftYonedaFix = id
>>> toList (lowerYonedaFix (liftYonedaFix xs))
[1,2,3]
```

#### Codensity / State isomorphisms

##### Theory

`Codensity m a = forall b. (a -> m b) -> m b` is the CPS
transform of a monad. For representable functors, Codensity
collapses to the State monad:

```
Codensity ((->) m)              ≅  State  m
Codensity (Compose ((->) m) n)  ≅  StateT m n
```

More generally, for any `Representable u` with `Rep u = r`:
`Codensity (Compose u n) ≅ StateT r n`.

```haskell
codensityToState  :: Codensity ((->) m) a -> m -> (a, m)
stateToCodensity  :: (m -> (a, m)) -> Codensity ((->) m) a
codensityToStateT :: Monad n => Codensity (Compose ((->) m) n) a -> StateT m n a
stateTToCodensity :: Monad n => StateT m n a -> Codensity (Compose ((->) m) n) a
```

##### Example 1: State round-trip

```haskell
import Data.Functor.Kan

>>> codensityToState (Codensity (\k m -> k (m + 1) (m * 2))) (10 :: Int)
(11,20)
>>> codensityToState (stateToCodensity (\m -> (m + 1, m * 2))) (10 :: Int)
(11,20)
```

##### Example 2: StateT round-trip

```haskell
import Data.Functor.Kan
import Control.Monad.Trans.State.Strict

let st = StateT $ \m -> Just (m + 1, m * 2) :: StateT Int Maybe Int
>>> runStateT (codensityToStateT (stateTToCodensity st)) 10
Just (11,20)
```

#### Re-exports

From kan-extensions: `Day`, `Yoneda`, `Codensity`,
`Ran`, `Lan`, `Density`, `Curried`,
`codensityToComposedRep`/`composedRepToCodensity`.

## Dependencies

```
base, data-fix, kan-extensions, transformers
```

All transitive dependencies of `kan-extensions` are already
pulled in by `profunctors`, so adding this package to a project
that uses `profunctors` adds exactly two packages (`data-fix` +
`scheme-extensions`).

## Relationship to other packages

| Package | Approach | This package |
|---|---|---|
| [recursion-schemes](https://hackage.haskell.org/package/recursion-schemes) | `Recursive`/`Corecursive` typeclasses | Explicit functions on `Mu`, no typeclasses |
| [data-fix](https://hackage.haskell.org/package/data-fix) | `Mu`/`Fix`/`Nu` + basic fold/unfold | Re-exported + extended scheme zoo |
| [kan-extensions](https://hackage.haskell.org/package/kan-extensions) | Day, Yoneda, Codensity, Ran, Lan | Re-exported + applied to recursion schemes |

Ported from `Control.Cirklon.Patn.Scheme` with attribution.
