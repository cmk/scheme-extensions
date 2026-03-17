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
foldGen         :: ... -> Mu f -> b                            -- gcata
unfoldGen       :: ... -> b -> Mu f                            -- gana
refoldGen       :: ... -> r -> b                               -- ghylo

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
comap           :: (Bifunctor f, ...) => (a -> b) -> Mu (f a) -> Mu (f b)
contramap       :: (Bifunctor f, ...) => (a -> b) -> Mu (f a) -> Mu (f b)
prepro          :: (forall a. f a -> f a) -> (f c -> c) -> Mu f -> c
postpro         :: (forall a. f a -> f a) -> (r -> f r) -> r -> Mu f
transverse      :: (forall a. f (g a) -> g (f a)) -> Mu f -> g (Mu f)
cotransverse    :: (forall a. g (f a) -> f (g a)) -> g (Mu f) -> Mu f
```

### Streaming metamorphisms (Gibbons)

Generic over the base functor and fixed-point type. Interleave
production and consumption so output can be emitted before all
input is consumed:

```haskell
stream  :: Functor f => (i -> g i) -> (f o -> o) -> ... -> state -> i -> o
astream :: Functor f => (i -> g i) -> (f o -> o) -> ... -> state -> i -> o
gstream :: Functor f => (i -> g i) -> (f o -> o) -> ... -> state -> i -> o
```

The caller passes project/embed functions, choosing the fixed-point
types for input and output:

```haskell
stream unwrap wrap ...       -- Mu -> Mu
stream unwrapNu wrapMu ...   -- Nu -> Mu (lazy in, strict out)
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
fstream  :: (i -> Cons b i) -> (Cons a o -> o) -> ... -> state -> i -> o

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
[kan-extensions](https://hackage.haskell.org/package/kan-extensions):

**Day-based structural comparison:**

```haskell
equalDay     :: (Foldable f, Eq1 f)  => Day f f Bool -> Bool
compareDay   :: (Foldable f, Ord1 f) => Day f f Ordering -> Ordering
recursiveEq  :: (Functor f, Foldable f, Eq1 f)  => Mu f -> Mu f -> Bool
recursiveOrd :: (Functor f, Foldable f, Ord1 f) => Mu f -> Mu f -> Ordering
```

**Yoneda hoist fusion** — multiple `hoistMu` calls fuse into
a single traversal:

```haskell
liftYonedaFix  :: Mu f -> YonedaFix f f
mapYonedaFix   :: (forall a. g a -> h a) -> YonedaFix f g -> YonedaFix f h
lowerYonedaFix :: YonedaFix f g -> Mu g

-- lowerYonedaFix . mapYonedaFix n3 . mapYonedaFix n2 . mapYonedaFix n1 . liftYonedaFix
-- = 1 traversal instead of 3
```

**Codensity / State isomorphisms:**

```haskell
codensityToState  :: Codensity ((->) m) a -> m -> (a, m)
stateToCodensity  :: (m -> (a, m)) -> Codensity ((->) m) a

-- Higher-kinded: Codensity (Compose ((->) m) n) ≅ StateT m n
codensityToStateT :: Monad n => Codensity (Compose ((->) m) n) a -> StateT m n a
stateTToCodensity :: Monad n => StateT m n a -> Codensity (Compose ((->) m) n) a
```

For any `Representable u` with `Rep u = r`:
`Codensity (Compose u n) ≅ StateT r n`.

**Re-exports** from kan-extensions: `Day`, `Yoneda`, `Codensity`,
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
