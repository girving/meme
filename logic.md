A logic for computation
=======================

We want a logic that mixes computational and noncomputational objects,
combining the expressiveness of ZFC with programming.  We'll start by
ignoring issues of soundness and universes, then attempt to fix this
later.  We also ignore issues of size (`sizeof`) by assuming that all
types are word sized as in ocaml.  I believe that fixes these issues
will be relatively easy.

More importantly, we assume a purely functional universe.  This assumption
is much harder to eliminate, but we have to start somewhere.

We use ASCII notation for easy translation into code.  In particular, we
use `:` (colon) for set membership, `<:` for subset, `->` for function
types, and `x => ...` for anonymous functions.

## Primitives

We have two universes `Set` and `Any`, where `Set <: Any`.  `Any` contains both
sets and immutable computational objects (booleans, lists, functions, etc.).
`Set` is the ZFC universe of sets that may include individuals.  We do not
assume the individuals include _only_ the computational objects, though that
would be a harmless assumption.

Aside: although `Set` lives in some sense outside the computational universe,
we allow pointers to point directly to sets, lists to include sets, etc.  At
machine runtime the logic will enforce that pointers to sets are never used, or
at most used for pointer equality comparison (TODO: Do we need the
qualification?).

The notion of a function is a bit slippery, since we want both the nice
properties of ZFC functions (concrete domain, conrete range, extensionality),
and first class support for partial vs. total and computable vs. noncomputable.
I believe we need two notions; since the ZFC notion is emergent from sets in
ZFC, only the second notion is necessary as a separate primitive.  We'll curry,
so actually what we want is not a notion of function, but of value.  Since we
allow pointers to reference sets, and general values have no properties that
sets don't, this second notion is just `Any`.

The primitives in our logic are

```
empty : Set        -- The empty set
Bool : Set         -- The set of booleans
false,true : Bool  -- The boolean individuals
(:) : Any          -- The set membership function
(==) : Any         -- Perfect structural equality over Any
all : Any          -- Universal quantification over a set:
                   --   if `a : Set, f : a -> Bool`, `all f a : Bool`
subsets : Any      -- Power set: if `a : Set`, `subsets a : Set`
filter : Any       -- Subset extraction
                   --   if `a : Set, f : a -> Bool`, `filter f a <: a`
choice : Any       -- Global choice: if `a : Set, a != empty`, `choice a : a`
safe : Any         -- Function safety at an argument:
                   --   if `f : Any, x : Any`, `safe f x : Bool`
```

Important: for now, we explicitly leave out a primitive notion of whether a
function is "computational" at a value (whether it terminates).  This is
because termination is not required for the logic to be sound, and on a
physical machine termination by itself is a weak construct.  Soundness matters,
and efficient execution matters, but little in between.  As with Milawa, we
bail at runtime if the system attempts to run a noncomputational function.

Notions of _approximate_ termination, as opposed to exact termination, are
useful pragmatic constructions, but these can be produced with simple fix point
calculation to check whether code obviously calls one of the noncomputational
primitives.

Based on the above primitives, we define a few derived notions:

```
-- Boolean not
!a = if a then false else true

-- Boolean and
a && b = if a then b else false

-- Boolean or
a || b = if a then true else b

-- Function types
a -> b = filter (f => all (x => safe f x && f x : b) a) Any

-- Existential quantification
any f a = !(all (x => !(f x)) a)

-- Function composition
f . g = (x => f (g x))

## Environments and knowledge

A proposition is simply a boolean value, together with its definition as code.
Code is algebraic datatype

```
data Code =
    Var Name
  | Let Name Code Code
  | Fun Name Code
  | App Code Code
  | If  Code Code Code
```

That's not going to be enough (we've left out both raw values and macros), but
we'll add further features as necessary.

An environment is a (finite) map from names to optional definitions.
Our knowledge is a set of codes that we know are true:

```
type Env = Map Name (Maybe Code)
type Known = Set Code
```

These are represented by ambient `env : Env` and `known : Known` variables.  I'm
not yet sure how these are accessed / interrogated.

Frequently, we'll have a function in `env` like `refl = x => x == x`, together with
an entry in `known` of the form `all refl Any` that fills in the missing universal
quantification telling us about the function.  Alternatively, we could just stick
the entire `all (x => x == x) Any` in `known`.

## Definitions and proofs

At any given point in the code, we have a number of definitions in `env` and some
related things in `known`.  We would like to know how to define new things, and
how to prove that some of these are true.  I believe the best version of this
involves deeply built in reflection, like Milawa.  However, as a warmup, we'll
start with a nonreflexive version and see what we can build up to.

Aside: names are wildly perceived to be problematic.  I am going to ignore these
issues for now, and assume that standard techniques will solve them.  It may also
help to name things by their content via cryptographic hashing.

### Nonreflexive propogation of env and known

Here is the obvious starter method for propagating `known` though some code:

```
analyze : Env -> Known -> Code -> Known
analyze e k c = case c of
  Var _ => k
  Let n x r => k = analyze e k x
               analyze (add e n \ Some x) k r
  Fun n x => analyze (add e n None) k x
  App _ _ => k
  If x y z => intersect (analyze e (add k   x)  y)
                        (analyze e (add k (!x)) z)
```

The `if` bit is basically cut, but the other rules of propositional logic are
unlikely to be included here.  Next step: how to do the rest as naturally as
possible.

Blah: let's dodge this issue entirely, by instead assuming a good proof checker
exists.

### The proof checker assumption

We assume two more primitive notions are available, a set `Proof` of proof
objects and a function `check` which checks that a proof ensures a definition
is true.  That is,

    Proof : Set
    check : Env -> Known -> Code -> Proof -> Bool

We leave the definition of `Proof` until later (it will be a simple algebraic
datatype), and assume that `check` is strong enough to verify all ZFC theorems.

### Axioms

The environment starts with an initial `Known` set of axioms.  We leave these
for later.

### Paradoxes

Let's imagine that we let `Any` include classes, but let sets contain any.

No!  Do not worry about paradoxes yet.  All in good time.

### Syntax

Our base syntax is a list of things that are true.  That is, it's a list of
expressions (`Code`) which conceptually evaluate to true, even if some of them
evaluate for true only because they are definitions.  We'll ignore
disambiguation for now.

Expressions can be either curried function application or binary operators.

Ug, this is all silly.  Syntax is a terrible drain on human thought.  Also a
highly addictive drug.  I really need to play with raw s-expressions for a
while.

Okay, rant out of the way.  Let's try to write a simple grammar, using
semicolons instead of indentation.

    tokens = name binop preop '-' ';'

    file = ...

No, *really* need to play with s-expressions for a while.
