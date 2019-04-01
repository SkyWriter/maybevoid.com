---
title: "Implicit Effects: Algebraic Effects in Haskell with Implicit Parameters"
---

<i>
  This is a draft post I am working on for releasing the Haskell library
  `implicit-effects`. For any feedback, please feel free to ping me through
  [email](mailto:soareschen@maybevoid.com) or drop a comment directly in the
  [latest commit](https://github.com/maybevoid/maybevoid.com/commits/master/site/drafts/implicit-effects.md).
</i>

## Introduction

In this post, I would like to introduce a new effects library called `implicit-effects`
to the Haskell community. `implicit-effects` is an _experimental_ effects library I
developed with less than a year study into algebraic effects. I will share about the
different approaches I use to implement algebraic effects in Haskell, which I think
is worth _considering_ or _explored further_ by the Haskell community. However
considering this is my first serious personal Haskell project, and given that I
lacks professional experience in developing production quality Haskell applications,
this is _not_ a call for adoption for you to use `implicit-effects` in any serious
Haskell projects. (At least not yet)

In the following sections, we will first go through a brief overview of the current
state of effects management in Haskell, and compare it to the full flexibility of
coding in algebraic effects. We then observe the repeating patterns used in
defining new effects, and learn about new concepts called operations and
co-operations. We will see how with implicit parameters, we can bind effect
operations into constraints without defining type classes. We then look at higher
level structures, such as `Computation`, `Handler`, and `Pipeline`, can help us
perform transformation on computations to give partial interpretation to effects.
Finally we look at some example programs in Eff and see how we can implement them
in Haskell using `implicit-effects`.

## From Monad to Extensible Effects

The concept of monad was borrowed from category theory to Haskell, with one of
its main use case is for allowing impure computation in a pure language using
the `IO` monad. Since then Haskellers have written numerous tutorials to explain
what monad is to the broader programming community. The abstraction behind monad
may be difficult to grasp for newcomers, but it is the functionalities provided
by implementing new monads that makes it worth learning deeper.

### Bare Monad

When we first started learning monads, we typically come across few of the well
known examples such as `Reader` and `State`, and they would look something as
follow:

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

As we implement more monads, a pattern arise and we find ourselves doing similar
things each time. We first define new datatypes such as `type Reader` or
`newtype State` to store the information required to carry out the monadic
operations containing the desired effects. After that we define the `Monad`
instance for these datatypes to give meaning to how `return` and `>>=` should
behave. To make it convenient to the library users, we also define convenient
functions such as `ask`, `get`, and `put` so that users can perform the
desired actions inside the monad without knowing the details. Finally we
define extraction functions so that we can extract a value `a` out of a
monad `m` for any `m a`.

An effect in its most basic form can be broken down into four parts:

**Effect Definition** - This defines the concept and "shape" of the effect
through its type constructor / kind signature. We can see that `Reader` is an
effect parameterized by a type `r`, while `State` is an effect parameterized
by a type `s`. Both `Reader` and `State` have the type/kind
`Type -> (Type -> Type)` - note the bracket on the right side is written
explicitly to show it produces a monad type, which have the type `Type -> Type`.
Lastly, also note that the body of the type definition is kind of irrelevant
from the user point of view. The author of an effect can choose to not expose
the data constructors without affecting the users of an effect.

**Effect Implementation** - This turns the datatypes we defined into a
_concrete_ effect by making it a monad instance. With that we can perform the
supported effectful computation under the given monad. In the example above,
the implementation also performs _interpretation_ of the effect through both
the `Monad` instance and the datatype bodies. The concrete effect is also
tied directly to the effect definition, which means we cannot define any
alternative implementation that share the same effect definition. We will
see in later sections how abstract implementations and free implementations
allows multiple implementations of the same effect definition.

**Effect Operation** - The effect operations can be seen as the external API
for an effect. For users of an effect, the only thing matter is the operation
functions associated with the effect definition. An effect operation may
accept zero or more arguments and return an effectful result wrapped under
the associated monad `m`. The body of the operation defines how the operation
is translated to the underlying concrete effect, but that detail is not important
to the user.

**Result Extraction** - Many effect implementations build up _contexts_ when
executing effectful computations, and then extract result from the final context
returned from the end of the computation. The context typically require
additional arguments for the result to be extracted, such as the initial state
for the State monad. In the above example, the context is defined directly in
the body of the effect definition. As a result the bind operator of the
`Monad` instances also have to take care of the monadic binding of the contexts.
Tight coupling between the context and monadic bind is one major source of
complexity, and we will learn in later sections how algebraic effects decouples
the context from the monad.

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
effects without implementing a `Monad` instance for it.

The concept of free monad also comes from category theory. Roughly
it means we can get a monad for free for any `Functor` instance `f`.
The `Free f` monad instance builds up a computation tree in `(>>=)`,
which then requires further interpretation to give actual meaning to
the computation.

```haskell
import Control.Monad.Free

-- From Control.Monad.Free
data Free f a
  = Pure a
  | Free (f (Free f a))

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

**Effect Interpretation** - Although the free monad is a _concrete_ effect,
it does not provide meaning directly to the computation. Instead it requires
_interpretation_ of the computation tree reified by the free monad. There
are many ways we can interpret an effect. The most common approach is to use
`foldFree`, which takes an interpreter with natural transformation
`forall x . f x -> m x` that interprets a _co-operation_ `f` under another
effect `m` for all continuation result `x`. The interpreter is used by
`foldFree` to perform catamorphism on `Free f a` and the result of the
interpretation becomes `m a`

**Free Effects** - Free effects are concrete effects in the sense that they
have a concrete implementation of a monad instance. This means unlike
abstract effects, computations that directly use a free effect are tied
to a particular implementation of free effects. Over the years there
are many free effects libraries published, including `free`,
`freer-simple`, `extensible-effects`, `fused-effects`, etc. Most of
the time, these libraries expect users to use the free effect they
offer for _all_ their application code. This often amplifies
concerns such as performance issues of free effects, as users are
locked in to using one free effect implementation and may not able
to switch out easily. A better approach would be to define
computations to use abstract effects such as `MonadState`, so that
they can still interprete the effects in other ways through either
alternative free effects or other concrete effects.

### Handler Pattern

Although free effects offer a lot of flexibilities in effect interpretation,
it also introduce additional complexities even in common cases that don't need
advanced effect interpretations. Consider a time effect that gets the current
system time:

```haskell
class Monad m => MonadTime m where
  currentTime :: m UTCTime
```

The `MonadTime` class provides an abstract interface to the time effect.
However implementing the class instance all possible monad transformer
stacks may prove to be a challenge to anyone without deep understanding
on typeclasses. The free effect approach may provide more flexibility,
but the boilerplate is not very intuitive and may look confusing.

```haskell
data TimeCoOp r = CurrentTime (UTCTime -> r)
  deriving (Functor)

type TimeEff e a = Free TimeCoOp a

interpTime :: forall s a . TimeEff a -> IO a
interpTime ref m = foldFree interp m
 where
  interp (CurrentTime cont) = getCurrentTime >>= cont
```

The use of free effects can get even more confusing when interpretation
of multiple effects are needed, adding additional noise to an effect
with rather simple default implementation.

People soon realize in most cases custom effects can be interpreted
directly in terms of an outer effect such as `IO`. In such cases
the complexities of monad transformers and free effects may cost
too much effort when all they need is `IO` with some mocking
capability.

The handler pattern is an alternative effect pattern that arise to
implement and interpret simple effects:

```haskell
-- Effect Operations
data AppOps eff = AppOps {
  currentTime :: eff UTCTime
  ...
}

defaultAppOps :: AppOps IO
defaultAppOps = AppOps {
  currentTime = getCurrentTime
  ...
}

mockAppOps :: AppOps IO
mockAppOps = AppOps {
  currentTime = return $ ... -- return mock time
  ...
}

app :: forall eff . AppOps eff -> eff ()
app ops = do
  ...
  time <- currentTime ops
  ...

defaultApp :: IO ()
defaultApp = app defaultAppOps
```

Using the handler pattern, there is almost no advanced Haskell feature
used, and the code becomes much easier to understand. Comparing more carefully
with the previous examples, we may notice there are still some similarities.

In the handler pattern, we are effectively sacrificing most of the power of
effect systems and keep only the effect operations. The `AppOps` type
is essentially the union of all simple effect operations that can implemented
using another effect `eff`, typically `IO`. We then pass around the effect
operations as function arguments, instead of passing them implicitly via
typeclass constraints.

Since the effect operations are passed explicitly, it is much easier to replace
the effect implementation. There is no need to declare a newtype wrapper around
the base effect just to allow for different typeclass instances. As we are
working mostly on a base effect like `IO`, we get to reason about our
applications much easily.

On the down side, the handler pattern makes it much harder if we want to
interpret or compose effects in more advanced ways. we want to wrap a `StateT`
around our base effect, we would at least need to redefine our effect operations
around that new base effect. Working with everything in `IO` also partially
forgo the advantage of coding in a purely functional language like Haskell,
as we can no longer reason about what exact side effects have happened when
our app returns a single `IO`.

Nevertheless, the simplicity of the handler pattern is worth considering.
What we really want to find out, is whether there is any way to implement
our effect operations similar to the handler pattern when it can be
interpreted with another base effect, without limiting the power of the
effect system? Ideally we'd like something as follow:

```haskell
data TimeOps eff = TimeOps {
  currentTime :: eff UTCTime
}

app1 :: forall eff . (Monad eff, MonadTime eff) => eff ()
app1 = ...

timeOps :: TimeOps IO {
  currentTime = getCurrentTime
}

-- withOps :: ???
app2 :: IO ()
app2 :: withOps timeOps app
```

As we will see in later sections, the "magical" `withOps` function is
one of the missing pieces we will implement to get a simple yet powerful
effect system with `implicit-effects`.

## Algebraic Effects

We are going take a detour away from Haskell and look at Eff, a research
language that implements algebraic effects. Algebraic effects is an emerging
programming language concept that is getting pushed for adoption in a number
of programming languages. It is similar to the effects systems and patterns
in Haskell, but is more generalized and more powerful. In languages such as
Eff, effect is a built in construct and thus require much less boilerplate
as compared to Haskell, which we have to encode effects using the existing
type system.

### State Effect in Eff

Let's look at an example definition and implementation of the state effect
in Eff:

```ocaml
(* Effect Definitions *)
effect Get : int
effect Put : int -> unit

(* Effect Handlers *)
(* state :: forall a ε . Int -> (() -> a!{Get, Put | ε}) -> (() -> a!{|ε}) *)
let state initial = handler
  (* Effect Interpretations *)

  (* Get (Int -> (Int -> a)) -> (Int -> a) *)
  | effect Get k -> (fun m -> continue k m m)

  (* Put Int (() -> (Int -> a)) -> (Int -> a)  *)
  | effect (Put n) k -> (fun m -> continue k () n)

  (* Context initialization *)
  (* return :: a -> (Int -> a)  *)
  | x -> (fun _ -> x)

  (* Result extraction *)
  (* finally :: (Int -> a) -> a *)
  | finally g -> g initial
;;

(* Computations *)
(* comp1 :: () -> Int!{Get, Put} *)
let comp1 () : int =
  let x1 = perform Get in
  perform (Put x1 + 1);
  x1 + 2
;;

(* comp2 :: () -> Int!{Get, Put} *)
let comp2 () : int =
  let x1 = comp1 () in
  let x2 = comp1 () in
  x1 + x2
;;

(* Effect handling *)
(* val res : int = 9 *)
let res = with state 2 handle comp2 ()
```

We can make several observations:

**Effect Definitions** - In Eff both the effect operations and co-operations
are defined together with the `effect` statement. The result type does not
need to be parameterized by a effect variable, as all functions in Eff can
invoke arbitrary effects in the body. When an effect is handled inside an
effect handler, it is passed in the co-operation form, e.g. `(Put n) k`
where k represents the continuation. When an effect is used by a computation,
it is called in the operation form, e.g. `(Put x)`, and the monadic result is
implicitly unwraped as return value to the caller.

**Effect Handlers** - In Eff an effect handler is defined using the `handler`
construct and can handle any number of effects whether they are related or not.
In the above example `Get` and `Put` are two separate effects that are handled
as a whole by the `state` effect handler. Here we have taken some liberty to
annotate the types of the Eff statements in a pseudo Haskell-like syntax
extended with the effect row notation `!{}`. Note that these are not actual type
signatures as the constructs are not regular functions in Eff.

The `state` effect handler have the conceptual type somewhere along the
line of `forall a ε . Int -> (() -> a!{Get, Put | ε}) -> (() -> a!{|ε})`.
It means given an initial state of type `Int`, and a computation thunk
`() -> a!{Get, Put | ε}` that would produce an effectful value of type `a`
that makes use of the effects `Get` and `Put` optionally with additional
effects `ε`, it returns a new computation thunk `(() -> a!{|ε})` that
no longer depends on the `Get` and `Put` effects when called.

The second last handler statement `x -> (fun _ -> x)` is the return handler
with the effective type `a -> (Int -> a)`. It wraps a pure value of type `a`
into a context of type `(Int -> a)` so that the context can be built up and
get called with the initial state by the finalizer. We can notice this is
pretty much the same as how the `State` monad in Haskell implements the
monadic `return`.

The first two handler statements handles the `Get` and `Put` effects.
They get access to the continuation `k` as the second argument. The handlers
extend the original context built up by the return handler and return a new
`Int -> a` in its result. There may be potential confusion when we then
try to determine the type of `k`. The first argument to `k` is the type of
the result of an effectful operation - `Int` for `Get` and `()` for `Put`.
But the result type of `k` is the context returned from the return handler,
`Int -> a`. In other words, the `Get` and `Put` effect handlers create a
new context `Int -> a`, which accepts for the current state value then calls
the continuation to get the inner context and then pass the new state to it.

The finalizer in the `finally` statement extracts the final result out of
the state context `g` of type `Int -> a` by calling it with the initial state
`initial`. This is how we get back our computation result `a` and is akin to
`evalState` of the `State` monad in Haskell.

**Computation** - In Eff any function can perform effectful computation, and
the expressions strongly resembles regular OCaml expressions. To call an
effect operation, the `perform` keyword is used in place of a function call.
Since `perform` also forms an expression, its return value can be used directly
in other expressions such as `let` without having to bind a monadic value
through `do` blocks or `(>>=)` like in Haskell.

Strictly speaking, `comp1` and `comp2` should have a type like
`() -> Int!{Get, Put}`. But in Eff the effects used by a function are not
tracked in its type signature. As a result we cannot generally know what
effects are used in a function. This makes Eff a static typed language with
dynamic effect system. As all effects in Eff must be handled when an expression
is evaluated, calling an unhandled effect would get handled by an unhandled
effect handler resulting in the program crashing.

The fact that all effects must be handled during evaluation is also why
`comp1` and `comp2` are functions of type `() -> Int!{Get, Put}` instead of
expressions of type `Int!{Get, Put}`. Since we don't want `comp1` and `comp2`
to bind with fixed effect handlers, we wrap them in a function so that the
effects can be handled by different effect handlers only when the functions are
called.

**Effect Handling** - In Eff effects are handled using the `with .. handle ..`
expression. The expression after `with` should be an expression of type
`handler` created from the `handler` expression. The expression after `handle`
should be an expression that is expected to invoke effect operations that
would get handled by the effect handler. Any effect that is not handled by
the handler in `with` would get propogated to the outer scope, until it is
handled or result in unhandled effect handler.

### Choice Effect in Eff

We can see from earlier example that with built in language constructs for
effects, we can express effects more elegantly in Eff than in Haskell. However
the full advantage of algebraic effects in Eff is not just the syntactic
constructs, but also new ways of structuring our code to solve different
problems. One particular strength is the access to the continuation `k` in
our effect handlers. Since we have access to the continuation, we can choose
to resume it more than one times, or discard the continuation and halting the
computation.

Consider the following Eff example for a choice effect:

```ocaml
(* Effect Definitions *)
effect Decide : bool;;

(* Effect Handlers *)
(* choose_true :: forall a ε . (() -> a!{Decide | ε}) -> (() -> a!{|ε}) *)
let choose_true = handler
  (* Decide (Bool -> a) -> a *)
  | effect Decide k -> continue k true;;

  (* default return *)
  (* return :: a -> a *)
  (* | x -> x *)

(* choose_all :: forall a ε . (() -> a!{Decide | ε}) -> (() -> [a]!{|ε}) *)
let choose_all = handler
  (* Decide (Bool -> [a]) -> [a] *)
  | effect Decide k -> continue k true @ continue k false

  (* return :: a -> [a] *)
  | x -> [x]
;;

(* comp1 :: () -> Int!{Decide} *)
let comp1 () : int =
  let x = (if perform Decide then 10 else 20) in
  let y = (if perform Decide then 0 else 5) in
  x - y
;;

(* val res1 : int = 10 *)
let res1 = with choose_true handle comp1 ()

(* val res2 : int list = [10; 5; 20; 15] *)
let res2 = with choose_all handle comp1 ()
```

The above code demonstrates an effect `Decide`, which non-derterministically
returns either `true` or `false`, akin to tossing a coin. Based on the result,
a computation can branch out in `if` statements to perform other compuations.
The interesting part is when the `Decide` operation is called, since the effect
handler have access to the continuation `k`, it can decide to resume the
continuation with _both_ `true` and `false`, and get back all possible results
of that computation.

Before going into multiple resumption of continuations, we take a look at
the simpler `choose_true` handler. `choose_true` always resume the continuation
exactly once with `true`, and return the result directly. Because `choose_true`
does not build any context or alter the result, it does not need to define a
return handler, which can otherwise be explicitly defined as `| x -> x`.
Since there is no context, `choose_true` also doesn't need a `finally` handler
as the continuation result is already `a`.

We then look at the `choose_all` handler, which always resume the continuation
with both `true` and `false`. This also means `choose_all` will get multiple
results from a computation `a`, each representing different branch results
from the `Decide` operation. In our example, `choose_all` returns all results
as `[a]` for any computation `a`.

In the return handler of `choose_all`, it wraps a plain value inside a
singleton list as the context, and has the type `a -> [a]`. This makes
the continuations in the `Decide` handler `[a]`, with the overall type
`Decide (Bool -> [a]) -> [a]`. The `Decide` handler resumes the
continuation twice with `true` and `false`, and then use the `@` operator
to concatenate the result for both branches.

Although `choose_all` builds up a context with `[a]`, it does not have
a `finally` handler to extract the context result back to `a`. Because
of this, `choose_all` _alters_ the result type of a computation `a` to
`[a]`, and have the type
`forall a ε . (() -> a!{Decide | ε}) -> (() -> [a]!{|ε})`. This shows
that unlike the free monad approach in Haskell, effect interpretations
in algebraic effects does not need to be natural transformation. An effect
handler can handle computations with concrete types, and produce new
computations with different types.

Finally we have an example computation `comp1`, which performs two `Decide`
operations, branch twice to decide the values for `x` and `y`, and finally
return the result `x-y`. In `res1` we handle `comp1` with `choose_true`,
which resume the continuation once with `true` for each call, and produce
the `Int` result `10`. Meanwhile in `res2` we handle `comp1` with `choose_all`,
which produces 4 different results: 10 for x=10 (true), y=0 (true); 5 for
x=10 (true), y=5 (false); 20 for x=20 (false), y=0 (true); and 15 for x=20
(false), y=5 (false).

As a final note, Haskell do have non determinism with monads such as `List`
corresponding to `choose_all`. However the effect interpretation is hardcoded
and does not allow us to easily switch to different interpretations such as
`choose_true`. In Eff since it is so easy to implement alternative effect
interpreations, we can for example build an alternative `choose_all` handler
that stores the result in a binary tree to represent the decision tree for
each decision.

When the effect context gets decoupled from the concrete monad, it becomes
much easier to define new contexts without having to worry about how `(>>=)`
can be derived from the context.

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