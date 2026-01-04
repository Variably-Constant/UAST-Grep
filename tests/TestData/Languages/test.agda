-- Agda Test File for UAST-Grep
-- Tests: data types, functions, pattern matching, proofs, modules

-- Module declaration
module test where

-- =============================================================================
-- Imports
-- =============================================================================

open import Agda.Builtin.Nat renaming (Nat to ℕ)
open import Agda.Builtin.Bool
open import Agda.Builtin.Equality
open import Agda.Builtin.List
open import Agda.Builtin.String

-- =============================================================================
-- Basic Data Types
-- =============================================================================

-- Simple enumeration
data Color : Set where
  red   : Color
  green : Color
  blue  : Color

-- Natural numbers (Peano style)
data Nat : Set where
  zero : Nat
  suc  : Nat → Nat

-- Boolean type
data Bool′ : Set where
  true  : Bool′
  false : Bool′

-- Maybe type (option type)
data Maybe (A : Set) : Set where
  nothing : Maybe A
  just    : A → Maybe A

-- Either type (sum type)
data Either (A B : Set) : Set where
  left  : A → Either A B
  right : B → Either A B

-- Pair type (product type)
data Pair (A B : Set) : Set where
  _,_ : A → B → Pair A B

infixr 4 _,_

-- List type
data List′ (A : Set) : Set where
  []  : List′ A
  _∷_ : A → List′ A → List′ A

infixr 5 _∷_

-- =============================================================================
-- Inductive Types with Indices
-- =============================================================================

-- Vector (length-indexed list)
data Vec (A : Set) : Nat → Set where
  []  : Vec A zero
  _∷_ : {n : Nat} → A → Vec A n → Vec A (suc n)

-- Finite numbers (bounded naturals)
data Fin : Nat → Set where
  zero : {n : Nat} → Fin (suc n)
  suc  : {n : Nat} → Fin n → Fin (suc n)

-- Binary trees
data Tree (A : Set) : Set where
  leaf : A → Tree A
  node : Tree A → Tree A → Tree A

-- Rose trees
data Rose (A : Set) : Set where
  rose : A → List′ (Rose A) → Rose A

-- =============================================================================
-- Functions
-- =============================================================================

-- Pattern matching on natural numbers
_+_ : Nat → Nat → Nat
zero    + n = n
(suc m) + n = suc (m + n)

infixl 6 _+_

_*_ : Nat → Nat → Nat
zero    * n = zero
(suc m) * n = n + (m * n)

infixl 7 _*_

-- Predecessor
pred : Nat → Nat
pred zero    = zero
pred (suc n) = n

-- Boolean operations
not : Bool′ → Bool′
not true  = false
not false = true

_∧_ : Bool′ → Bool′ → Bool′
true  ∧ b = b
false ∧ _ = false

_∨_ : Bool′ → Bool′ → Bool′
true  ∨ _ = true
false ∨ b = b

infixr 6 _∧_
infixr 5 _∨_

-- If-then-else
if_then_else_ : {A : Set} → Bool′ → A → A → A
if true  then x else y = x
if false then x else y = y

-- =============================================================================
-- List Functions
-- =============================================================================

-- Length
length : {A : Set} → List′ A → Nat
length []       = zero
length (x ∷ xs) = suc (length xs)

-- Append
_++_ : {A : Set} → List′ A → List′ A → List′ A
[]       ++ ys = ys
(x ∷ xs) ++ ys = x ∷ (xs ++ ys)

infixr 5 _++_

-- Map
map : {A B : Set} → (A → B) → List′ A → List′ B
map f []       = []
map f (x ∷ xs) = f x ∷ map f xs

-- Filter
filter : {A : Set} → (A → Bool′) → List′ A → List′ A
filter p []       = []
filter p (x ∷ xs) with p x
... | true  = x ∷ filter p xs
... | false = filter p xs

-- Fold
foldr : {A B : Set} → (A → B → B) → B → List′ A → B
foldr f z []       = z
foldr f z (x ∷ xs) = f x (foldr f z xs)

-- =============================================================================
-- Vector Functions (Length-indexed)
-- =============================================================================

-- Head (total function for non-empty vectors)
head : {A : Set} {n : Nat} → Vec A (suc n) → A
head (x ∷ xs) = x

-- Tail
tail : {A : Set} {n : Nat} → Vec A (suc n) → Vec A n
tail (x ∷ xs) = xs

-- Vector append (length types match)
_++v_ : {A : Set} {m n : Nat} → Vec A m → Vec A n → Vec A (m + n)
[]       ++v ys = ys
(x ∷ xs) ++v ys = x ∷ (xs ++v ys)

-- Lookup with safe indexing
lookup : {A : Set} {n : Nat} → Vec A n → Fin n → A
lookup (x ∷ xs) zero    = x
lookup (x ∷ xs) (suc i) = lookup xs i

-- =============================================================================
-- Proofs and Propositions
-- =============================================================================

-- Propositional equality
data _≡_ {A : Set} (x : A) : A → Set where
  refl : x ≡ x

infix 4 _≡_

-- Symmetry of equality
sym : {A : Set} {x y : A} → x ≡ y → y ≡ x
sym refl = refl

-- Transitivity of equality
trans : {A : Set} {x y z : A} → x ≡ y → y ≡ z → x ≡ z
trans refl refl = refl

-- Congruence
cong : {A B : Set} {x y : A} → (f : A → B) → x ≡ y → f x ≡ f y
cong f refl = refl

-- Proof: 0 + n = n
+-identity-left : (n : Nat) → zero + n ≡ n
+-identity-left n = refl

-- Proof: n + 0 = n (requires induction)
+-identity-right : (n : Nat) → n + zero ≡ n
+-identity-right zero    = refl
+-identity-right (suc n) = cong suc (+-identity-right n)

-- Proof: addition is associative
+-assoc : (m n p : Nat) → (m + n) + p ≡ m + (n + p)
+-assoc zero    n p = refl
+-assoc (suc m) n p = cong suc (+-assoc m n p)

-- =============================================================================
-- Higher-Order Functions and Polymorphism
-- =============================================================================

-- Identity function
id : {A : Set} → A → A
id x = x

-- Composition
_∘_ : {A B C : Set} → (B → C) → (A → B) → A → C
(g ∘ f) x = g (f x)

infixr 9 _∘_

-- Flip arguments
flip : {A B C : Set} → (A → B → C) → B → A → C
flip f y x = f x y

-- Apply
_$_ : {A B : Set} → (A → B) → A → B
f $ x = f x

infixr 0 _$_

-- =============================================================================
-- Records
-- =============================================================================

-- Point record
record Point : Set where
  constructor mkPoint
  field
    x : Nat
    y : Nat

-- Open record (bring fields into scope)
open Point

-- Usage example
origin : Point
origin = mkPoint zero zero

moveRight : Point → Point
moveRight p = record p { x = suc (x p) }

-- Monoid record
record Monoid (A : Set) : Set where
  field
    ε   : A
    _·_ : A → A → A
    -- Laws would go here in a full implementation

-- =============================================================================
-- Modules
-- =============================================================================

module NatOps where
  double : Nat → Nat
  double n = n + n

  triple : Nat → Nat
  triple n = n + n + n

-- Parameterized module
module ListOps (A : Set) where
  sum-length : List′ A → List′ A → Nat
  sum-length xs ys = length xs + length ys

-- =============================================================================
-- Instance Arguments (Type Classes)
-- =============================================================================

record Eq (A : Set) : Set where
  field
    _==_ : A → A → Bool′

open Eq {{...}}

instance
  EqNat : Eq Nat
  EqNat = record { _==_ = eqNat }
    where
      eqNat : Nat → Nat → Bool′
      eqNat zero    zero    = true
      eqNat (suc m) (suc n) = eqNat m n
      eqNat _       _       = false

-- =============================================================================
-- With Abstraction and Pattern Guards
-- =============================================================================

-- Example using with
min : Nat → Nat → Nat
min m n with m | n
... | zero  | _     = zero
... | _     | zero  = zero
... | suc m′ | suc n′ = suc (min m′ n′)

-- Rewrite
+-comm-zero : (n : Nat) → n + zero ≡ n
+-comm-zero n rewrite +-identity-right n = refl
