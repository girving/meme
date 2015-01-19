Noncomputational types
======================

There is a universe Set.  For now Set : Set, even though that is nonsense.

There are functions

  x : T -> y

If, given x : T, y : S, then

  (x : T -> y) : Pi(x : T -> S)

There are natural numbers.  That is,

1. Nat : Set
2. 0 : Nat
3. S : Nat -> Nat

There is a boolean type

  Bool = false | true

There are subset types.  Given T and f : T -> Bool, we can form

  sub T f

Rules about immutable memory
============================

Memory consists of blocks, and the blocks consist of either word-sized integers
or pointers.  That is, we have types

  type Word
  type Entry = Data Word | Ptr (n : Nat) (Mem n)
  type Mem (n : Nat) = Block (Fin n -> Entry)

There is a type Word.
