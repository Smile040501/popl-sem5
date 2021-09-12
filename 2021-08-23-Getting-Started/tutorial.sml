(** * A tutorial to get started with Standard ML.


This is a tutorial to get you started with standard ml. You should
have with you all the tools required for the compiler course.

By now you would have realised that anything that starts with "(*" and
ends with "*)" is a comment. Fortunately, these comments nest unlike the
case of /* */ in C.

So we have our first lesson which is to write comments.


*)


(* Let us define some values and see how sml responds to it. *)

val aString     = "Hello  world"
val aChar       = #"c"
val aBool       = true
val anotherBool = false
val anInt       = 42
val aReal       = 42.0
val aList       = [1,2,3]

val aPair       = ("the answer to life, universe, and everything", 42)
val aTriple     = (1, 2.4, "hello")
val aUnit       = () (* empty product type *)

(*

Languages like C or Javascript do not take their types very seriously
in the sense that C allows (even implicitly does) conversion between
types like int, double and pointers. Such is not the case with SML and
in this sense SML is strongly typed. The types are also checked at
compile time (well in the case of sml compilers like mlton that is)
unlike the language like Python or ruby. In this sense SML is
statically type checked language. Despite these, one almost never
needs to explicitly give the type declarations. The compiler "infers"
the type using the context.  What this means is that we get all the
advantages of strong and static type checking, i.e. safety and speed,
without paying its price in verbosity. This is one of the powerful
features that makes programming in ML a pleasant experience.

Exercise: Enter the above definitions on the sml interpreter and
note the types that the interpreter prints.

*)

(*
Answer:

val aString = "Hello World" : string
val aChar = #"c" : char
val aBool = true : bool
val anotherBool = false : bool
val anInt = 42 : int
val aReal = 42.0 : real
val aList = [1,2,3] : int list

val aPair = ("the answer to life, universe, and everything",42) : string * int
val aTriple = (1,2.4,"hello") : int * real * string
val aUnit = () : unit

*)


(*

We can also provide the type as the next example shows. In the
definition below the origin : real * real asserts that origin is a
constant of type real * real.

*)

val origin : real * real = (0.0, 0.0)

 (* Exercise: What happens with the following definition ? *)
 (* val origin : real * real = (0.0, "0.0") *)

 (*
    Answer: We will get an error saying that the val declaration don't match with the value
            pattern: real * real
            expression: real * string
 *)



 (* We can give new names to types. This is called type aliasing  *)
type vector2d = real * real

val unitx : vector2d = (1.0,0.0)          (* unit vector along x-axis *)
val unity : vector2d = (0.0,1.0)          (* unit vector along y-axis *)





(*

** Functions.

Functions are what makes programming in sml great. Let us start by
defining the simplest factorial function. Since function application
is a very common operation, they have a very simple syntax: applying f
on x is just f x

*)

fun fact n = if n <= 0 then 1
	     else n * fact (n - 1) (* Notice the recursive use of fact *)

(** The idea of currying.

In ML we only have functions with one arguments. To illustrate let us
look at the following variants of add.

*)

fun add (u,v) = u + v
fun addp u v  = u + v

(*

Both the functions only take one argument each:

val add : int * int -> int

val addp : int -> (int -> int)

The first variant takes a parameter of type (int * int), i.e. a tuple
and returns an int.  The second variant takes a single int parameter
and returns a function that takes a single int parameter and returns
int.


Functional programmers call the variant add as the uncurried version
and addp as the curried version.


The advantage of the curried version is that we can "partially" apply
addition.

*)

val increment = addp 1

(*

In general a function that takes n parameters of types t1, t2 .. tn
and returns a value t can be thought of as the type t1 -> (t2 ->
(.. -> tn)). This is called currying. Currying is a powerful idea that
allows such partial applications.


We can convert from one curried version to its uncurried version and
vice-versa as follows.


*)


fun curry   f  x y  = f (x,y)	(* convert to curried form   *)
fun uncurry f (x,y) = f x y    (* convert to uncurried form *)


val addp1 = curry  add           (* Notice the use of val instead of fun *)
val incr  = addp1 1
val add1  = uncurry addp

(*
    Other way to curry/uncurry:
    
    fun curry f = fn a => fn b => f (a,b);

    fun uncurry f = fn (a, b) => f a b;
*)


(*

The functions above illustrate an important aspect of Sml, Functions
are truly first class as they can be passed to other functions or
stored in a data structure like list

*)

val someIntFunctions = [addp 1, addp 2, addp 3]

(*

If functions are truely first class there should be a way of creating
functions without defining them using a `fun` definition. ML allows us
to that using what are (now) known as anonymous functions. Such
functions were first explored by Church in his lambda calculus
(https://en.wikipedia.org/wiki/Lambda_calculus).

*)


val increment = fn x => x + 1 (* we revisit the increment function *)

(*

The ML expression `fn x => (...x...)` is that function that maps its
argument x to (...x...)  With annonymous function, we can rewrite some
functions definitions. For example, the following definitions all mean
the same thing.

fun f a b c = e
fun f a b   = fn c => e
fun f a     = fn b => fn c => e
val f       = fn a => fn b => fn c => e


Exercise: Rewrite curry and uncurry using annonymous
functions. i.e. Fill in the blanks below.

fun curry f x = fn y => f (x, y)
fun curry f   = fn x => fn y => f (x, y)
val curry     = fn f => fn x => fn y => f (x, y)

fun uncurry f = fn (x, y) => f x y
val uncurry   = fn f => fn (x, y) => f x y

*)

(** * List functions and pattern matching

In ML lists are written as follows

*)

val firstFewOddPrimes = [3,5,7]


(*

The x :: xs denotes a list with x as its first element and xs as the
rest of the list

*)

val firstFewPrimes = 2 :: firstFewOddPrimes


(*

Here is an example of a list function that applies a given function to
all the elements on the list

*)

fun map f []         = []
  | map f (x :: xs)  = f x :: map f xs


(*

Let us see what happens if we increment the primes: Silly program
but illustrates the use of map and partial application.

*)

val useless = map incr firstFewPrimes

val somemorestuff = map (addp 42) firstFewPrimes (* See the use of currying *)


(* Another function that is very useful  for processing lists are folds

fold f a [b0, b1 , b2, b3 ...] = f (f a b0) b1 ....

Think of op as an operator we have

fold op a [b0,b1,b2 ...] = ((a op b0) op b1) op b2 ...

corresponds to the library function foldl

*)


fun fold _ x []      = x
  | fold f x (y::ys) = fold f (f x y) ys


(* Let us write a function to sum up a list of numbers. Notice that this is just a fold *)

val sum = fold addp 0  (* sum [x1,x2,x3..] = ((0+x1) + x2) ....) *)


(*

Let us write the product function which takes a product of the
list. Again it turns out to be just a fold

*)

val prod = let fun mul x y = x * y
	   in fold mul 1
	   end

(*

 We can use this to define factorial for which we first define the
 enumerate function

*)

fun enum a b = if a <= b
	       then a :: enum (a + 1) b
               else []

fun factorial n = prod (enum 1 n)

val fct = prod o enum 1  (* using function composition o *)

(*

Exercise: Our "folding" is from left, we could define a right fold as
well which does the folding from right

i.e  fold op [a,b,c,...z] b =  a op (b op ... (y op (z op b)))


If the operator o is not commutative this can be different.

The standard library has its own variants of foldr and foldl but they
accept the functions in uncurried form. Have a look at their types.


*)

(*
  val foldr = fn : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
  val foldl = fn : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b
*)

fun foldr _ x []      = x
  | foldr f x (y::ys) = f y (foldr f x ys);



(*  Parameteric Polymorphism.

We already seen the smartness of  the sml compiler/interpreter in infering
the type of your values/functions from the context. If you look at the type
that the compiler infered for your map function you would have noticed something
strange.

val map = fn:  ('a -> 'b) -> 'a list -> 'b list.

Here the 'a and 'b are type variables, i.e. can be any type you
wish. In plain English it means the following: Let 'a and be 'b be any
types. map takes a function from 'a to 'b to a function that map an 'a
list to 'b list.  The 'a list type itself is polymorphic and sml has
interpreted the most general type for the map function that is
consistant with its definition. This kind of polymorphism is called
parametric polymorphism in the sense that map is a generic list maping
function that works no matter what the types 'a and 'b are.

Internally, sml infers the "most general type" by solving a set of
constraints that arise by the definition. If it is not able to solve
it means that there is a type error. Here are some simple examples.

*)

fun identity x    = x         (* val id :  'a -> 'a                                 *)
fun constant x y  = x         (* val const : 'a -> 'b -> 'a                         *)
fun compose f g x =  f (g x)  (* val compose : ('b -> 'c) -> ('a -> 'b) -> 'a -> 'c *)


(** Algebraic data types.

Algebraic data type allows us to captures abstract syntax of languages
pretty easily and thus are particularly well suited for writing
compilers. We start with few examples.

*)

datatype Colour = Red | Blue | Green

datatype DaysOfTheWeek = Sunday | Monday | Tuesday | Wednessday | Thursday | Friday | Saturday


(*

Algebraic data types capture types that are defined by a (finite) set
of rules or cases which build the type from simpler objects. The rules
or cases are distinguished by the constructors of the types. For
example, the colours datatype says that a colour is formed by three
rules distinguished by the constructors Red, Blue and Green. In the
case of DaysOfTheWeek there are 7 constructors.

The constructors have two roles. Firstly, they create values of that
type.

*)


val skyColour    = Blue   (* skyColour has type Colour *)
val favouriteDay = Sunday (* favouriteDay has type DaysOfTheWeek *)


(*

The constructors of a type is also used to define functions from the
type by using what is known as pattern matching as given below.

*)

fun isHoliday Sunday = true
  | isHoliday _      = false

(*

   The above function says which of the days of the week is a
   holiday. It should be read as follows as a set of two rules.

   1. Sunday is a holiday
   2. Anything else is not.


   You can think of the function as trying out each of the cases and
   seeing which of the cases match. Here, _ is a match everything
   pattern often called wildcard pattern.

*)

(** * Recursive types

   Algebraic data types can be recursive and can define very
   interesting data types. Let us define the list type ourselves even
   though the standard library already has one. Let us fix a type 'a
   (notice the polymorphism). Mathematically a list is defined using
   two rules.

   1. An empty list is a list. Call this the nil rule
   2. If xs is an 'a list and x is of type 'a then the tuple (x, xs)
      is a list. Call this the cons rule.

   We can capture this in ML as follows.

 *)


datatype 'a mylist = nil
		   | cons of ('a * 'a mylist)


(* We can define functions again by pattern matching. *)
fun isEmpty nil = true
  | isEmpty _   = false

fun listLength nil            = 0
  | listLength (cons (_,xs))  = 1 + listLength xs

(*

The listLength function can be seen as defining the length of the list
using two rules

1. The length of a nil list is 0

2. The length of a list of the kind (x,xs) is 1 + the length of the
list xs.

The only difference between our list and the standard list there is
some syntactic sugar where [] is for nil and the infix constructor ::
for cons.

*)

(* In general an algebraic datatype will look like this

datatype ('a1, 'a2, 'a3) Name = C1 of (...) | C2 of (...) | C3 ...

Here C1, C2 C3 etc are constructors.


A pattern for the datatype would look like

1. A wild card pattern `_` (underscore) which matches any value

2. A variable pattern say `x` which like the wild card matches any
value but it also binds the value to the variable `x` so that x can be
used on the right hand side of the definition.

3. C p where C is a constructor and p is some pattern. This will
   match any value that looks like the constructor C applied on an
   expression e ...  where the pattern p matches e.

Just like values, the tuples and lists have special patters.


1. Built in patterns for list like ([]), (p1 :: p2), and sequences of
   patterns like [p], [p1,p2], [p1,p2,p3] etc.  where pi's are
   themselves patterns.

2. Built in pattern for tuples like (), (p), (p1,p2) etc.

A function defined using a list of pattern matching rules "tries" each
pattern and gives out the value corresponding to the first case that
matched.

The option type is a useful datatype defined in standard library that
is used to signal optional result. They capture a lot of logic that
involves the use of the unsafe null values (null pointer in C, null in
Java). They are defined as

datatype 'a option = SOME of 'a | NONE

We use this to define the head and tail of a list.

 *)

fun head []         = NONE
  | head (x::_)     = SOME x

fun tail []         = NONE
  | tail (_ :: xs)  = SOME xs

(*

Pattern matching using case expression.

Sometimes it is natural define values using pattern matching cases as
the following rewrite of head illustrates.

*)

fun head1 xs = case xs of
		   []      => NONE
		 | x :: xs => SOME x

(*

Let us end by writing a main function that is the entry point for our
tutorial.

*)

fun main _ = let val (question,answer) = aPair
	     in map print [question, ": ", (Int.toString answer), "\n"]; 0 end
