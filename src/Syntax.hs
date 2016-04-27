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
-- error. If you start an expression or declaration on a column, you can
-- continue that expression or declaration on a new line as long as you
-- indent a little more.

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
-- `Ord` (ordering comparisons), and `Show` (printing as a debug string)
-- type classes.
data PersonC = Adult' String Int | Child' String
  deriving (Eq, Ord, Show)

-- Haskell provides a *record syntax*. This works just like a normal data
-- declaration, and it also generates functions for accessing the data. It
-- can be a convenient way to write "getters" for data at the same time as
-- you define the type. 
--
-- One catch is that, since the record fields are just generated functions,
-- you can't reuse the field names, and they can't be the same as any other
-- function in the module.
data Person = Person { personName :: String, personPowerLevel :: Int }

-- The above declaration makes the `Person` type, with a `Person`
-- constructor that takes a `String` and an `Int` parameter. It also
-- generates two functions, `personName` and `personPowerLevel`, each of
-- which take a `Person` as a parameter and return the `String` and `Int`.

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
-- name `vegeta` to be the PersonA as descirbed.

vegeta = PersonA "Vegeta" 9000

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
-- types. Let's write a function to add 1 to a person's power level. I'm
-- going to write a function here that is idiomatic Haskell, and I'll
-- follow up with an annotated version.

powerUp :: PersonA -> PersonA
powerUp (PersonA name powerLevel) = PersonA name (powerLevel + 1)

{-
+--------+--------------------- powerUp' 'has-the-type'
|        |  +------------------ Takes a PersonA as argument
|        |  |       +---------- Function arrow, can read as "to"
|        |  |       |  +------- Returns a PersonA    
V        V  V       V  V        -}
powerUp' :: PersonA -> PersonA
--        +-------------------- Pattern match on the Constructor
--        |       +----+------- Bound parameters
--        V       V    V
powerUp' (PersonA name powerLevel) =
--  +-------------------------- Reconstruct a person
--  |       +------------------ Reuse the old name
--  |       |     +------------ Add one to power level
--  V       V     V         
    PersonA name (powerLevel + 1)

-- Haskell is immutable, so we never really modify data in place. Most
-- functions in Haskell deconstruct data using pattern matching, and
-- reconstruct it to do the job.

-- Let's get the name of a person. Try this exercise on your own.
getName :: PersonA -> Name
getName = error "Write me!"

-- Functions that take multiple arguments use the same arrow. The last
-- thing in the type is the return type of the function. This function
-- allows one person to absorb another person's power. The underscore in
-- the second pattern match indicates that we don't care about that
-- variable, and won't bind it.
absorbPower :: PersonA -> PersonA -> PersonA
absorbPower (PersonA name oldPower) (PersonA _ otherPower) =
    PersonA name (oldPower + otherPower)

-- Uh oh! Haskell values are immutable, and we're only returning one new
-- PersonA. That means the old person still has all their power! The
-- Haskell solution is to return *two* parameters, using a *tuple*. Tuples
-- are a fixed length list of data types that can be of different types.
--
-- This function also introduces a *let expression*. We'll demonstrate the
-- record accessor functions that we made for the regular Person type.
absorbPower2 :: Person -> Person -> (Person, Person)
absorbPower2 person victim =
    let absorbedPower = personPowerLevel person + personPowerLevel victim
        newPerson = Person (personName person) absorbedPower
        drainedVictim = Person (personName victim) 0
    in (newPerson, drainedVictim)

-- There are two important bits here: `let` and *binding priority*.
--
-- `let ... in` allows us to define a bunch of intermediate terms that are
-- only in scope for the expression following `in`.
--
-- Functions bind tighter than any infix operator. So the expression:
--
--     personPowerLevel person + personPowerLevel victim
--
-- is equivalent to
--
--     (personPowerLevel person) + (personPowerLevel victim)
--

-- There's another common way of declaring common expressions, using
-- a `where` block. `let` is an expression and the new terms are only in
-- scope in the expression of `in`, while a `where` declaration introduces
-- them for the whole function declaration.
--
-- Rewriting the above function to use `where` looks like:
absorbPowerWhere :: Person -> Person -> (Person, Person)
absorbPowerWhere person victim =
    (newPerson, drainedVictim)
  where
    absorbedPower = personPowerLevel person + personPowerLevel victim
    newPerson = Person (personName person) absorbedPower
    drainedVictim = Person (personName victim) 0
    
-- Alright, enough with people. Let's get into some more *functional*
-- programming stuff. Singly linked lists make an awesome introduction to
-- thinking about problems recursively and functionally. Let's write some
-- functions for working with them.

-- First, let's write a function that takes the first element of a list.
-- This is called `head` in the Prelude for regular lists.

myHead :: List a -> a
myHead (Cons a _) = a
-- Uh oh: what if we call it with an empty list? Haskell will crash at
-- runtime with an error message: "missing pattern match on myHead: Nil".
-- And, in fact, if you try to compile with only this definition, the
-- compiler will issue a warning: 
--
--      Pattern match(es) are non-exhaustive in
--      an equation for `myHead`: Patterns not matched: Nil
-- 
-- If you write a function that can crash at runtime like this, it's best
-- to provide an informative error message.
myHead Nil = error "myHead called with an empty list."
-- This gets rid of the warning, but runtime failures are annoying. We can
-- avoid this problem with `Maybe`:
--
-- We're also going to introduce our first *control flow* expression here.
-- The `case` expression is used to pattern match on the value and provide
-- different paths based on the different constructors.

safeHead :: List a -> Maybe a
safeHead list =
    case list of
         Cons a _ -> Just a
         Nil -> Nothing

-- This function will *never* crash at runtime -- it's called *total*, and
-- that's a really desirable property.

-- One common thing we want to do in Functional-Land is *map* over a list.
-- Mapping over a list takes a function, a list, and returns a new list
-- that is the result of applying the function it to every element in
-- the input list.
--
--      +----- This argument is a *function* passed in
--      V
myMap :: (a -> b) -> List a -> List b
-- Here we have the *base case* for working with lists: the empty list! If
-- we have an empty list, then we can't apply the function to anything. So
-- we *return* an empty list.
myMap _ Nil = Nil
myMap f (Cons a as) = Cons b bs
  where
    -- Now, we have a function `f :: a -> b`, and a value `a`. We need
    -- a value of type `b`. We can apply `f` to `a` to get this.
    b = f a
    -- Next up, we have a value `as :: List a`, and we need a value 
    -- `bs :: List b`. We can use `myMap` to provide that!
    bs = myMap f as

-- Passing functions to `myMap` is easy-peasy. We've got two options: we
-- can pass a partially-applied function, or we can make a lambda
-- expression (aka anonymous function).
oneTwoThree :: List Int
oneTwoThree = Cons 1 (Cons 2 (Cons 3 Nil))

twoThreeFour :: List Int
twoThreeFour = myMap (\x -> x + 1) oneTwoThree

-- Lambda expressions start with a `\`, introduce the arguments, and start
-- the expression with `->`. In ES5 JavaScript, this looks like:
--
--     function(x) { return x + 1; }
--

strings :: List String
strings = myMap show oneTwoThree

-- Haskellers use `map` so often that they've defined a special infix
-- operator for it: <$>, which you can read as "mapped over"
--
--     function <$> list
--
-- reads as "function mapped-over the list." In truth, this function is
-- generalized to work over a bunch of different types, not just lists. 
-- That's beyond this right now.
--
-- Let's make our own infix operator for myMap! Infix operators need to
-- have *only* symbols, but they can be defined just like functions.
-- Indeed, they *are* just functions.

{-
+------ To refer to an operator without any arguments, you surround it with
|       parentheses
V  -}
(<%>) :: (a -> b) -> List a -> List b
func <%> xs = myMap func xs
--   ^
--   +-- Operators can be defined infix.

-- Next up, we'll implement filter. Filter is used to take a list and
-- produce a new list that only has the objects satisfying some property.

--         +--------  This function takes an `a` and returns `True` or
--         |          `False`
--         V
myFilter :: (a -> Bool) -> List a -> List a
-- Like `map`, filtering an empty list returns just an empty list.
myFilter _ Nil = Nil
-- For the recursive case, we have to check the object and see if it makes
-- the predicate return true. We'll use an `if` expression. One important
-- thing to note is that Haskell `if` *requires* that you specify an `else`
-- clause!
myFilter p (Cons a as) =
    if p a
       then Cons a (myFilter p as)
       else myFilter p as
-- Functional programmers often prefer to express things in terms of
-- *guards*, rather than *if* expressions. Guards look like this:
myGuardFilter :: (a -> Bool) -> List a -> List a
myGuardFilter _ Nil = Nil
myGuardFilter p (Cons a as)
    | p a = Cons a (myGuardFilter p as)
    | otherwise = myGuardFilter p as
-- The `|` introduces a predicate, and if the predicate is true, then that
-- definition is used. `otherwise` is an alias for `True`, which always
-- triggers. The alternatives are tried top-to-bottom, so you have to
-- consider overlapping guards.

-- Filtering is also pretty common, but we don't have an infix operator for
-- that in Haskell usually. Let's define one:
(</>) :: (a -> Bool) -> List a -> List a
(</>) p list = myFilter p list
-- We can use operators as prefix by surrounding them with parentheses.

-- Since Haskell is so functional, we often want to compose functions
-- together. Let's write that:

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g a = f (g a)

-- Haskellers like to pretend that we're mathemeticians, and give the
-- smallest syntax to the most important/common ideas. For that reason, the
-- `.` operator is used for function composition. Here's a somewhat
-- contrived example:

contrivedCompose :: Int -> Int
contrivedCompose = add 5 . multiply 10
  where
    add = (+)
    multiply = (*)
    -- If we expand out the definitions above:
    aka = \a -> add 5 (multiply 10 a)

-- Composition is all about defining a pipeline where the values from from
-- left to right through the functions. A long chain of composition, like
--     
--     composed = foo . bar . baz . quux
-- 
-- is generally nicer than 
--    
--     composed x = foo (bar (baz (quux x)))

-- The last operator we'll need to introduce is `$`. `$` is an infix
-- function that lets you use fewer parentheses. It's defined like:

($) :: (a -> b) -> a -> b
f $ x = f x

infixr 0 $

-- For any operator, we can define the associativity and precedence.
-- $ associates to the right, which means that we can take a chain of `$`s
-- and have implied parentheses grouping them to the right:
--
--     dollah x = foo $  bar $  baz $ quux x
-- 
-- is the same as:
--
--     parens x = foo $ (bar $ (baz $ quux x))
--
-- The precedence tells us in which order to evaluate infix operators. So
-- a precedence of 0 means *last*, and 9 means *first*. Regular prefix
-- function application is the highest precedence, so it gets evaluated
-- first.
--
-- This is just like how we order arithmetic operations. The expression
--
--     5 * 4 + 10 + 3 * 6
--
-- has an implied parentheses of
--
--     (5 * 4) + 10 + (3 * 6)
--
-- since multiplication binds tighter than addition.

----------------------------------------------------------------------------
--                      Do Syntax                                         --
----------------------------------------------------------------------------

-- Okay, so at this point, you know how to read and write data
-- declarations, functions, and expressions. Haskell has one last bit of
-- syntax you need to learn in order to be productive. This is `do` syntax,
-- introduced by the `do` keyword. We'll do some examples with `IO`, the
-- type of input/output in Haskell.

--           +----- This means we're doing IO
--           |  +-- () signifies that we aren't returning anything
--           V  V
helloWorld :: IO ()
helloWorld = do
    putStrLn "Hello Worlding so hard rn"
    putStrLn "so functional wow"

-- You can read `IO a` as "An IO action that, when executed, returns
-- a value of type `a`."

-- The `do` block sequences the two prints, so when we call this function
-- from `main`, it'll print these two strings out.
-- Let's get some input now:

prompt :: IO String
prompt = do
    putStrLn "Enter a text pls:"
    line <- getLine
    pure line

-- Alright, we've used a new symbol: <-. This lets us `bind` the value from
-- the thing on the right to the name on the left. `getLine` has the type:
--
--     getLine :: IO String
--
-- and, when executes, waits for the user to input a line and returns that
-- to you. So when we use the arrow with `getLine`, we take the `IO
-- String`, run the action, and `bind` the name `line` to the `String`
-- inside the `IO`.

-- The last line in a `do` block must have the type of the last thing. So
-- if we wrote:
--
--     prompt :: IO String
--     prompt = do
--       putStrLn "Enter a text"
--       line <- getLine
--       line
-- 
-- we'd get a type error! This is because `line` has the type `String`, and
-- the last line in a `do` block must have the type `IO String`. We can use
-- the `pure` function to take any *pure* value and lift it into the `IO`
-- type.
--
-- We can use `let` in a `do` block like you might expect.

customPrompt :: String -> IO String
customPrompt string = do
    let promptStr = string ++ "> "
    putStrLn promptStr
    getLine

-- Note that we didn't use `<-` to get the `line` out of `getLine`. The
-- last line in the block has to match the type `IO String`, which
-- `getLine` already does.

-- That should be all the Haskell syntax you need to get started. Go have
-- fun!
