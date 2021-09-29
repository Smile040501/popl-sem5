(*
# References in Standard ML..

Dead line : 4th October 2021.

In this assignment, we use the `ref` construct of Standard ML to
create a counter. SML provides first class references to create a
mutable cell. The syntax of it is as follows

```
val x = ref 0   (* create a reference cell with initial value 0 *)
val _ = x := 42 (* store 42 inside the reference cell in the variable x *)
val vx = !x     (* get the value out of the reference cell *)

```

Also we have the `;` operator with the following syntax `e1 ; e2`
reduces `e1` first and then reduces `e2`. The value of `e1; e2` is the
value of `e2`.
*)

(*
1. Define a Counter structure that has an internal ref cell and
   exposes the three functions, `incr : unit -> unit`, `decrement :
   unit -> unit`, `get : unit -> int`. The outside world should not
   have any other access to the counter.  Hint: You will have to
   define a signature say COUNTER and restrict he signature of your
   Counter structure appropriately.
*)
signature COUNTER =
sig
   val incr : unit -> unit
   val decr : unit -> unit
   val get  : unit -> int
end

structure Counter :> COUNTER =
struct
   val num = ref 0
   fun incr () = (num := !num + 1)
   fun decr () = (num := !num - 1)
   fun get  () = !num
end



(*
2. What if your program requires two or more counter ? Instead of a
   plain `Counter` structure define a `MkCounter` functor which
   creates a structure of the previous kind. This way you can have
   multiple counters.

```
	   structure A = MkCounter ()
	   structure B = MkCounter ()

	   A.incr ()
	   B.decr ()
	   ...
```
*)
functor MkCounter () :> COUNTER =
struct
   val num = ref 0
   fun incr () = (num := !num + 1)
   fun decr () = (num := !num - 1)
   fun get  () = !num
end

(*
## Bonus question (Ungraded)

In compilers we often need to compare variables, store them in data
structures like set, maps etc. Variables are represented as strings in
program and we could directly use it. However, to speed up computation
of equality and ordering, we could instead represent variables as
integers. On most processors, integer equality/comparison just takes a
single instruction where as for strings it will take multi-byte
comparison each of which could cost 1 instruction. However,
representing variables as integers is pretty bad when it comes to
human usability.

The [`Atom` module][atom] provides an elegant approach for solving
this problem. The idea is that we assign integer names to variables
and use these integer values in the data structures that the compiler
process. Only when we pretty print it out to the user do we use the
string associated with the integer. We can accomplish this using three
references

1. A counter that is used to assign a integer name to a variable when
   it is first encountered.

2. A integer to string lookup table `I2S` say allows looking up string
   values for a given integer.

3. A string to integer lookup table `S2I` say that allows looking up
   the integer code for all the strings seen so far.

The `I2S` and `S2I` maps should be inverses of each other. You can use
an [`ORD_MAP` data structure][ordmap] for the key value lookup table.

The idea is simple. There are three `ref` variables that contians the
above three elements.  The functions like `atom` on input `s` first
looks up `S2I` to see if `s` is already allocated a code (of type
`atom`).  If yes it just returns that code. If no it increments the
counter and assignes this new code to `s` by updating `I2S` and `S2I`
appropriately. A similar strategy is used by the `toString` function.

### Type safety through opaque signature

The `Atom.atom` type is really `int` but you would want to prevent
users from given `toString` function an arbitrary int (you might not
have assigned any string that code). You can use the opaque signature
technique to ensure that `Atom.atom` type is different from int
outside the `Atom` structure. Any atom value that you can generate is
only through the controlled interface provided by the function `atom`.

[atom]: <https://www.classes.cs.uchicago.edu/archive/2015/spring/22620-1/atom-sig.html>
[ordmap]: <https://www.classes.cs.uchicago.edu/archive/2015/spring/22620-1/ord-map-sig.html>
*)