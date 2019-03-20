---
title: "Implicit Effects: Algebraic Effects in Haskell with Implicit Parameters"
---


## Introduction

In this post, I would like to introduce a new effects library called `implicit-effects` to the Haskell ecosystem.
`implicit-effects` is an _experimental_ effects library I developed with less than a year study into algebraic
effects. I will share about the different approaches I use to implement algebraic effects in Haskell, which I
think is worth _considering_ or _explored further_ by the Haskell community. However since this is my first
serious personal Haskell project, and given that I lacks professional experience in developing production-grade
Haskell applications, this is _not_ a call for adoption for you to use `implicit-effects` in any serious Haskell
projects. (at least not yet)

In the following sections, we will first go through a brief overview of the current state of effects management
in Haskell, and compare it to the full flexibility of coding in algebraic effects. We then observe the repeating
patterns used in defining new effects, and learn about new concepts called operations and co-operations. We will
see how with implicit parameters, we can bind effect operations into constraints without defining type classes.
We then look at higher level structures, such as `Computation`, `Handler`, and `Pipeline`, can help us perform
transformation on computations to give partial interpretation to effects. Finally we look at some example
programs in Eff and see how we can implement them in Haskell using `implicit-effects`.

## From Monad to Extensible Effects

The concept of monad was borrowed from category theory to Haskell, with one of its main use for introducing
impure computation to the pure language through the `IO` monad. Since then Haskellers have written numerous
tutorials to explain what monad is to the broader programming community. The abstraction behind monad may
be difficult to grasp for newcomers, but it is the functionalities provided by implementing new monads
that makes it worth learning deeper.

### Bare Monad

When we first started learning monads, we typically come across few of the well known examples such as
`Reader` and `State`, and they would look something as follow:

```haskell
-- Effect Definitions
type Reader r a = r -> a
newtype State s a = State { runState :: s -> (a,s) }

-- Effect Implementations
instance Monad ((->) r) where
  return x = \_ -> x
  h >>= f = \w -> f (h w) w

instance Monad (State s) where
  return x = State $ \s -> (x,s)
  (State h) >>= f = State $
    \s -> let (a, s') = h s
              (State g) = f a
          in  g s'

-- Effect Operations
ask = id
get = State $ \s -> (s,s)
put s = State $ \s -> ((),s)

-- Result Extractions
runReader = ($)
evalState = fst . runState
```

As we implement more monads, a pattern arise and we find ourselves doing similar things each time.
We first define new datatypes such as `type Reader` or `newtype State` to store the information
required to carry out the monadic operations containing the desired effects. After that we define
the `Monad` instance for these datatypes to give meaning to how `return` and `>>=` should behave.
To make it convenient to the library users, we also define convenient functions such as `ask`, `get`,
and `put` so that users can perform the desired actions inside the monad without knowing the details.
Finally we define extraction functions so that we can extract a value `a` out of a monad `m` for any
`m a`.

An effect in its most basic form is consist of four parts:

**Effect Definition** - This defines the concept and "shape" of the effect through its type
constructor / kind signature. We can see that `Reader` is an effect parameterized by a type
`r`, while `State` is an effect parameterized by a type `s`. Both `Reader` and `State` have
the type/kind `Type -> (Type -> Type)` - note the bracket on the right side is written
explicitly to show it produces a monad type, which have the type `Type -> Type`. Lastly,
also note that the body of the type definition is kind of irrelevant from the user point of
view. The author of an effect can choose to not expose the data constructors without
affecting the users of an effect.

**Effect Implementation** - This turns the datatypes we defined into a _concrete_ effect by
making it a monad instance. With that we can perform the supported effectful computation
under the given monad. In the example above, the implementation also performs _interpretation_
of the effect through both the `Monad` instance and the datatype bodies. The concrete effect
is also tied directly to the effect definition, which means we cannot define any alternative
implementation that share the same effect definition. We will see in later sections how
abstract implementations and free implementations allows multiple implementations of the
same effect definition.

**Effect Operation** - The effect operations can be seen as the external API for an effect.
For users of an effect, the only thing matter is the operation functions associated with
the effect definition. An effect operation may accept zero or more arguments and return
an effectful result wrapped under the associated monad `m`. The body of the operation
defines how the operation is translated to the underlying concrete effect, but that
detail is not important to the user.

**Result Extraction** - Many effect implementations build up _contexts_ when executing
effectful computations, and then extract result from the final context returned from the
end of the computation. The context typically require additional arguments for the
result to be extracted, such as the initial state for the State monad. In the above
example, the context is defined directly in the body of the effect definition. As a
result the bind operator of the `Monad` instances also have to take care of the
monadic binding of the contexts. Tight coupling between the context and monadic
bind is one major source of complexity, and we will learn in later sections how
algebraic effects decouples the context from the monad.

### Monad Transformers

Effects defined with the bare monad approach as described in the earlier section
is relatively easy to understand, but it suffers from non-composability of monads.
What happen if we want to perform an effectful computation that uses operations
from both the `Reader` and `State` monad? We wouldn't able to reuse the code we
defined earlier, and instead we would have to define a whole new effect definition
that includes both operations, and scratch our heads on how to implement `(>>=)`
for the combined contexts.

Fortunately with monad transformers, we gain back some extensibilty we lost from
using bare monads. We would instead define our reader and state effects as
`ReaderT` and `StateT`.

```haskell
-- Concrete Effects
newtype ReaderT r m a = ReaderT {
  runReaderT :: r -> m a
}

newtype StateT s m a = StateT {
  runStateT :: s -> m (a, s)
}

-- Abstract Effect
class Monad m => MonadReader r m | m -> r
 where
  -- Effect Operations
  ask :: m r

-- Abstract Effect
class Monad m => MonadState s m | m -> s
 where
  -- Effect Operations
  get :: m s
  put :: s -> m ()
```

In the monad transformer approach, each effect definition is parameterized by
an effect `m` in addition to the result type `a` of a bare monad. A monad
transformer `t` basically takes any monad `m` and form a new monad `t m`.
With some `FlexibleInstances` derivations through the `MonadTrans` class,
the transformed monad `t m` supports all the existing effects and operations
of the inner monad `m` in addition to new effects and operations introduced
by `t`.

There are also some new concepts introduced:

**Concrete Effects** - `ReaderT` and `StateT` are now definitions of concrete
effects. They still define the contexts in the body and implement the `Monad`
instance, but users of the effects no longer need to access them directly.

**Abstract Effects** - `MonadReader` and `MonadState` are definitions of
_abstract_ effects. The effects are defined as a typeclass, with its
associated operations defined as methods of the class. This allows
multiple concrete effects to be implemented for a given abstract effect.
Computations that uses a `MonadState s` effect for example do not need
to care whether it is running under a `MonadState s (ReaderT r m)` or a
`ReaderT r (MonadState s m)`, or even a custom `MyStateEff s` monad.

The monad transformers approach is well known and battle-tested, in
particular with the MTL library. It provides moderate extensibility
through type classes, and is optimized over time to have high performance.
However defining new effects through typeclasses is not always
straightforward, requiring users to have intricate knowledge of
various GHC extensions to implement or even use a given effect under
their specific monad. The limitation of typeclasses also makes it
difficult to implement some of the algebraic effects examples
using monad transformers alone.

### Free Monad

The monad transformer approach provides a somewhat extensible way to
create custom effects. However it still have a tight coupling
between the _concrete_ effect, _interpretation_ of the effect,
and the _context_ for the interpretation. In order to give new
meaning or interpretation to an abstract effect, we must define
a new datatype and provide at least instances of `Functor`,
`Applicative`, and `Monad`.

Because of this, Haskellers tend to conflate between the concepts
of _monad_ and _effects_. It is common that defining new effects
is synonymous to defining datatypes with `Monad` instances. Free
monad changes this by making it possible to create and interpret
effects without implementing a `Monad` instance.

The concept of free monad also comes from category theory. It means
we can get a monad for free for any `Functor` instance `f`. The
`Free f` monad instance builds up a computation tree in `(>>=)`,
which then requires interpretation to give actual meaning to
the computation.

```haskell
import Control.Monad.Free

-- Effect Co-operations
data EnvCoOp e r
  = Ask (e -> r)
  deriving (Functor)

data StateCoOp s r
  = Get (s -> r)
  | Put s (() -> r)
  deriving (Functor)

-- Free Effects
type EnvEff e a = Free (EnvCoOp e) a
type StateEff s a = Free (StateCoOp s) a

-- Effect Operations

-- EnvOps
instance MonadReader e (EnvEff e) where
  ask = liftF $ Ask id

-- StateOps
instance MonadState s (StateEff s) where
  get = liftF $ Get id
  put x = liftF $ Put x id

-- Effect Interpretations
interpEnv :: forall e a . e -> EnvEff e a -> a
interpEnv e m = iter interp m
 where
  interp (Ask cont) = cont e

interpState :: forall s a . IORef s -> StateEff s a -> IO a
interpState ref m = foldFree interp m
 where
  interp (Get cont) = readIORef ref >>= cont
  interp (Put x cont) = writeIORef ref x >>= cont
```

Comparing to monad transformers, free monad introduces a few
new concepts that makes the code look cleaner:

**Co-Operation** - A free effect is defined around the
_co-operation_ of an effect. A co-operation is dual to the operation
of an effect - for instance if we stare and compare the co/operations
of `StateEff` for long enough, we would make the following observation:

  - `StateOps` is the product of the operations `get` and `put`, while
    `StateCoOp` is the sum of the co-operations `Get` and `Put`.

  - The argument types for operations in `StateOps` are joined with
    `(->)` forming a function, while the argument types for co-operations
    in `StateCoOp` are joined with `(,)` forming a tuple.

  - The result type for an operation in `StateOps` indexed by value type `a`
    is the free effect `StateEff s a` returned by the operation functions;
    The result type for a co-operation in `StateCoOp` indexed by value type `a`
    is the _continuation_ `(a -> r)` for _any_ r type in `StateCoOp s r`.


## References

  - [Effects bibliography](https://github.com/yallop/effects-bibliography), Jeremy Yallop
  - [Programming with Algebraic Effects and Handlers](http://math.andrej.com/wp-content/uploads/2012/03/eff.pdf),
    Andrej Bauer and Matija Pretnar
  - [An Introduction to Algebraic Effects and Handlers](http://www.eff-lang.org/handlers-tutorial.pdf),
    Matija Pretnar
  - [What is algebraic about algebraic effects and handlers?](https://arxiv.org/pdf/1807.05923.pdf),
    Andrej Bauer
  - [Freer monads, more extensible effects](http://okmij.org/ftp/Haskell/extensible/more.pdf),
    Oleg Kiselyov and Hiromi Ishii
  - [Shallow Effect Handlers](http://homepages.inf.ed.ac.uk/slindley/papers/shallow-extended.pdf),
    Daniel Hillerström and Sam Lindley

  - [Freer Monads: Too Fast, Too Free](https://reasonablypolymorphic.com/blog/too-fast-too-free/),
    Sandy Maguire
  - [Free Monad Benchmarks](https://github.com/feuerbach/freemonad-benchmark), Roman Cheplyaka