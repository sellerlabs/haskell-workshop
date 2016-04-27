-- First, we need comments. Haskell has two styles of comments: these
-- line comments, prepended by a --.

{- 
Block comments look like this. They can be nested and span multiple lines.
They can also be placed inline of code, and they're often used for
"language pragmas."
 -}

-- This file (and presentation) goes into Haskell syntax more than you
-- might expect for a "intro to $LANGUAGE" workshop. Most languages in
-- common use today inherit their syntax from C, FORTRAN, or Lisp.
-- Haskell's syntax is *very* different, and your intuitions from other
-- languages won't carry over.

-- Every Haskell source file defines a module. If you don't specify one,
-- then the compiler assumes that it's a `Main` module, with a `main`
-- function that can be executed. That's not the case here, so we provide
-- a module declaration:
module Syntax where


-- First thing is first: Haskell is *indentation sensitive*, like Python.
-- Top level declarations must start at column 0. Otherwise, it's a parse
-- error.


-- The importings bits aren't super important to know, so feel free to skim
-- this and use it as a reference if you need later.
--
-- Next up, we can declare some imports. Haskell implicitly imports a small
-- standard library called the Prelude. Since we'll be re-implementing some
-- bits of it in this exercise, we're going to explicitly import only what
-- we need, and hide the rest. A declaration like:
-- 
--     import Prelude
-- 
-- brings the entire Prelude module into scope, so the names can be
-- referred to unqualified. This is equivalent to Python's 
--     
--     from Prelude import *

-- We can explicitly import only what we want using this syntax. This
-- declaration imports some stuff we'll use later. This syntax is
-- equivalent to Python's:
--
--     from Prelude import Int, String, Eq, Ord, Show
--
import Prelude (Int, String, Eq, Ord, Show)

-- For the rest, we'll do a qualified import. This lets us refer to terms
-- in the Prelude like P.head, P.String, etc.
import qualified Prelude as P

-- The "top level" of a Haskell source file is a bunch of declarations. The
-- first set that we'll talk about are *data declarations*. This is how you
-- create new data structures and types.

---------------------------------------------------------------------------
--                        Data Declarations                              --
---------------------------------------------------------------------------

-- If you're coming from another programming language, you're used to
-- reading `=` as 'assignment'. Haskell uses `=` to mean `is defined as`.

--   +--------------------------- Name of the *type*
--   |         +----------------- Name of the *data constructor*
--   |         |       +--------- Takes a `String` parameter
--   |         |       |      +-- Takes an `Int` parameter
--   V         V       V      V
data PersonA = PersonA String Int 

-- First, we use the `data` keyword to specify that we're defining a new
-- type of data. All concrete types start with a capital letter, and the
-- name of this type is PersonA. We define the data constructor `PersonA`,
-- which takes two parameters: one of type `String` and another of type
-- `Int`. Data constructors are used just like functions.
--
-- We can think of a PersonA as being "a string *and* an int." Often, we
-- also want to say "or." We can do this in Haskell.

--   +---------+-----+------+--------------- As above
--   |         |     |      |   +----------- "Or"
--   |         |     |      |   | +--------- Data constructor
--   |         |     |      |   | |     +--- Parameter
--   V         V     V      V   V V     V 
data PersonB = Adult String Int | Child String

-- The above example defines two data constructors that fit in the type of
-- `PersonB`. A `PersonB` can be an `Adult` with a `String` and `Int`, *or*
-- a `Child` with just a `String`.

-- Unlike many languages, Haskell doesn't provide equality testing,
-- comparison, or printing functions for arbitrary data types. Haskell uses
-- an system called 'type classes' to provide overloaded functions, and you
-- must opt-in for these data types. Many type classes can be derived. The
-- `deriving` keyword below derives instances for the `Eq` (equality),
-- `Ord` (ordering comparisons), and `Show` (printing as a string) type
-- classes.
data PersonC = Adult' String Int | Child' String
  deriving (Eq, Ord, Show)

-- Generics are fun and easy in Haskell!  One of the most common examples
-- of this is the `Maybe` type. Maybe is our first example of a generic
-- type.
--
--   +------------------------ Name of the *type constructor*
--   |      +----------------- Type parameter
--   |      |   +------------- `Just` constructor
--   |      |   |     +------- Parameter of type `a`.
--   |      |   |     | +----- "or"
--   |      |   |     | | +--- Constructor named `Nothing`
--   V      V   V     V V V
data Maybe' a = Just' a | Nothing'

-- There are apostrophes (often read as 'prime') to distinguish this from
-- the real Maybe imported in the Prelude. Note that the generic type
-- parameter is lowercase -- in types, lowercase always means a variable.
-- You can use any lowercase identifier you want, though `a`, `b`, etc. are
-- conventional for arbitrary generic types.

-- Because they're just so darn good at teaching the basic fundamentals of
-- functional programming, we'll define the type of singly linked lists
-- here, to use in the rest of our examples.
data List a = Cons a (List a) | Nil
  deriving (Eq, Show)

-- We can read that data declaration as: "I'm defining a new data type
-- called List that takes a single type parameter a. List has two
-- constructors, one Nil that doesn't take any parameters, and Cons, which
-- takes a value of type `a` and a value of type `List a`.

---------------------------------------------------------------------------
--                           Value Declarations                          --
---------------------------------------------------------------------------

-- Let's start by declaring constant values. This line of code defines the
-- name `person` to be equal to be Vegeta.

person = PersonA "Vegeta" 9000
--
-- If you just put two things together, Haskell assumes that you're trying
-- to apply values to a function. Data constructors are functions, so you
-- can apply "Vegeta" and 9000 to `PersonA` to get `person`.

-- That number probably caught you a little off guard. Usually, when you
-- see `Person String Int` you expect it to be the person's name and age
-- (or, at least, I do). You can make *type synonyms* like this:

type PowerLevel = Int
type Name = String

-- and now the `Person` type could be expressed as
--     
--      data Person = Person Name PowerLevel
--

-- We can provide a type annotation for a declaration like this:

goku :: PersonA
goku = PersonA "Goku" 9001

-- The symbol `::` is read as "has-the-type", so we can read the above
-- declaration as: "The value `goku` has-the-type PersonA." Haskell has
-- complete type inference, so you never need to write a type signature if
-- you don't want to. It's a "best practice" to write type signatures for
-- top level declarations, and they can be helpful while developing as
-- assertions to the compiler that you expect something to be a certain
-- type. This can help the compiler infer correct types for the rest of the
-- expression.

-- Let's write our first function! Functions are first class values in
-- Haskell, and we can do anything with them that we can with other data
-- types. As a quick warm up, let's write our own `add` for convenience:


