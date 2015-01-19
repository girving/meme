// A simple, inconsistent, dependently typed language

#include <stdint.h>

// Let's try the subtyping route, and see where it takes us.
// In fact, let's try to be as "untyped" as possible, and build
// up types out of function calls.

// We start with a single, implicit type of values.  We'll leave it unnamed for now.

// There is a notion of function pointer.  x is a function pointer iff
//
//   (fun? x)
//
// This definition is cyclic, and indeed we have (fun? fun?).
// The above contains an implicit "= true"; that is, the following are equivalent
//
//   P iff (fun? x)
//   P iff (== true (fun? x))

// We have a few basic types, again represented as predicates:
//
//   word?    - 63-bit unsigned integers, represented as 1|(n<<1)
//   char?    - Characters, represented as words
//   string?  - Strings, represented as cons lists of characters (words)

// Expressions (exp?) are one of the following, written as form | needs
//
//   (var s)     | (string? s)
//   (word w)    | (word? w)
//   (let s x y) | (string? s) (exp? x) (exp? y)
//   (pair x y)  | (exp? x) (exp? y)
//   (call f x)  | (exp? f) (exp? x)

// Not all expressions are safe.  Safety involves a context c:
//
//   (safe? c (var s))       iff    (has? c s)
//   (safe? c (word w)       iff    true
//   (safe? c (let s x y))   iff    (safe? c x) & (safe? (add c s x) y)
//   (safe? c (pair x y))    iff    (safe? c x) & (safe? c y)
//   (safe? c (call f x))    iff    (takes? c f x)

// Expressions are one of the following, where s : String, w : Word, x y : Exp.
//   (var s)
//   (word w)
//   (single x)

//   (let s x y)  // let s = a in b

//   (pair x y)
//   (fst x)
//   (snd x)

//   nil
//   (cons p)       // If p : (Pair t (List t)), (cons p) : (List t)
//   (ifnil s x z)  // If s = nil, s : Unit in x, otherwise x : Pair in y.

//   (call x y)
//   (fun s x y)  // s : x -> y

//   (open s x y) // If x : (Sum t f), s = the nonexistent value


// We start with a single universe
//
//   Value = The type of 64-bit words, either pointers or integers.
//
// We then cyclically state that:
//
//   (fun? x) iff x is a function pointer
//   (fun? fun?) = true
//   (fun? bool?) = true
//   (bool? (fun? v)) = true
//   (bool? x) = true iff x = true or false 
//
// 

// We have the following basic types:
//
//   Value         - A 64-bit word, representing something.
//   Word <: Value - A 64-bit word, representing an unsigned integer.
//   Pair <: Value - A pointer to a pair of type (Value,Value)
//   Fun <: Value  - A function pointer of type Value -> Value
//   Bool <: Value - False
//
// We identify subsets with their characteristic functions.  That is,
//
//   Word = 
//
// Each of these types is itself a Type, which is a kind of Value.  That is,
//
//   Value,Word,Pair,Fun : Type <: Value
//
// Later, we will encode Type as an algebraic datatype.

// There is a general notion of refinement type.  If A <: 
//
// There is one more kind of Type, called a 

// If p : Fun, and 

// There is a type Value, represented in memory as one word.

// Each value has a type, and the type is one of the following:
//   Word - A 63-bit unsigned integer, stored as (n<<1)|1 (a la ocaml).
//   (Single t x) - The singleton type consisting solely of value x, which has type t.
//   (Pair a b) - A pointer to a heap allocated cons cell of type (a,b).
//   (List a) - Lists of type a, represented as cons cells.
//   (Sum t f) - If t : Type, f : (Closure t Type), a nonexistent value x : t,
//               and actually the type f(x).  In other words, a pair (x,y) where x isn't stored.
//   (Pi t f) - If t : Type, f : (Closure t Type), the dependent function type x : t -> f(x).
//              Represented as a function pointer, not a closure (no state).
typedef void* Value;
typedef _AnyPair { Value fst; Value snd; } AnyPair;
typedef Value (*AnyFun)(Value);

// Unit is just 0.
//   Unit = (Single Word 0)
// let unit = (single 0)

// Natural numbers are lists of Unit:
//   Nat = List Unit

// Strings are lists of Words:
//   String = List Word

// Closures are pairs of functions and their data.  If t : Type, f : t -> Type,
//   (Closure t f) = (Sum Type (x -> Pair x (Pi (Pair x t) (Apply f x))))
//   (Closure t f) = (Sum Type (x -> Pair x (Pi (Pair x t) (Apply f x))))
// where
//   (ClosureHelper (pair (pair t f) x)) = (Pair x (Pi (Pair x t) (call f x)))

// Closures are pairs of functions and their data.  If t : Type, f : t -> Type, then
//   (Closure t f) = (Sum Type ct   (Sum x (Pi (Pair x t) b))
// Note that the type c is not actually stored.

// Expressions (Exp) are one of the following, where s : String, w : Word, x y : Exp.
//   (var s)
//   (word w)
//   (single x)

//   (let s x y)  // let s = a in b

//   (pair x y)
//   (fst x)
//   (snd x)

//   nil
//   (cons p)       // If p : (Pair t (List t)), (cons p) : (List t)
//   (ifnil s x z)  // If s = nil, s : Unit in x, otherwise x : Pair in y.

//   (call x y)
//   (fun s x y)  // s : x -> y

//   (open s x y) // If x : (Sum t f), s = the nonexistent value

// The polymorphic identity function
//   id : (Pi 

// An Any is a consistent type, value pair:
//   Any = (Sum Type 

// Environments consist of (string,type,value) pairs.
//   Env = (List (Pair String (Exists 

// There is a prelude environment consisting of a number of name,value pairs:

/*
 * The types above are represented as Values as follows:
 *   (value Type) = 0
 *   (value Word) = 1
 *   (value Unit) = 2
 *   (value (Pair a b)) = (pair 0 (pair a b))
 *   (value (Fun a b)) = (pair 1 (pair a b)) 
 *   (value (Union a b)) = (pair 2 (pair a b))
 *   (value (List a)) = (pair 3 a)
 */

/* Expressions are built up out of the following primitives */

/* Construction and destruction of pairs */
static Value pair(Value a, Value b) {
  AnyPair p = (AnyPair)malloc(sizeof(_AnyPair));
  p->fst = a;
  p->snd = b;
  return p;
}
static Value fst(AnyPair p) {
  return p->fst;
}
static Value snd(AnyPair p) {
  return p->snd;
}

/* Induction on lists */
