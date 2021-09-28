(*
# Folds and Recursion.

Deadline: 20th Sep 2021, 11:59 pm

In the last lab we talked about the `map` function. Another important
function is the `fold` function (also known as `reduce`) which takes a
list an computes a "summary" of it. There are two variants of it

```
foldr : ('elem * 'summary -> 'summary) -> 'summary -> 'elem list -> 'summary
foldl : ('elem * 'summary -> 'summary) -> 'summary -> 'elem list -> 'summary

```

The semantics of these functions are the following.

```
foldr sfun s₀ [x₀, x₁, ... , xₙ] = f (x₀ , f (x₁, ... f (xₙ, s0))
foldl sfun s₀ [x₁, x₁, ... , xₙ] = f (xₙ , f (x₁, f ( x0, s0)))
```

We use the summarising function `sfun` to compute the summary of the
list starting with an initial summary `s₀`. The two variants differ
from which "side" the summary computation is started.  It is easier to
seem the semantics if for a moment think of the summarising function
`sfun` as an operator `⛒`.

(Note that I have tweaked the type of foldl to make it more readable
in this context) `


```
foldr : ('elem * 'summary -> 'summary) -> 'elem list  -> 'summary -> 'summary`
foldr ⛒ [x₀, x₁, ... xₙ] s₀ =  x₀ ⛒ (x₁ ⛒ (... xₙ₋₁ ⊗ (xₙ ⛒ s₀)))


foldl : ('elem * 'summary -> 'summary) -> 'summary -> 'elem list -> 'summary`
foldl ⛒ s₀ [x₀, x₁, ... xₙ] = (((s₀ ⊗ x₀) ⊗ x₁) ...⊗ xₙ₋₁) ⊗ xₙ
```
*)

(* =============================================================================================== *)
(*
1. Define the functions `foldr` and `foldl` using the pattern matching
   for list.
*)

(* val foldl = fn : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b *)
fun foldl _ x []        = x
  | foldl f x (y :: ys) = foldl f (f (y, x)) ys;


(* val foldr = fn : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b *)
fun foldr _ x []        = x
  | foldr f x (y :: ys) = f (y, (foldr f x ys));
(* =============================================================================================== *)

(* =============================================================================================== *)
(*
2. Without using pattern matching, define the function `sum : int list
   -> int` that computes the sum of a list of integers.
*)

(* val sum = fn : int list -> int *)
fun sum xs = let
                 fun add (u, v) = u + v
             in 
                 foldl add 0 xs
             end;
(* =============================================================================================== *)

(* =============================================================================================== *)
(*
3. Instead of using explicit recursion, define the following library
   function in terms of either `foldr` or `foldl` which ever is
   convenient. For the documentation of these library function, read the
   documentation of the [`List`
   structure](http://sml-family.org/Basis/list.html)

   - `partition : ('a -> bool) -> 'a list -> 'a list * 'a list`

   - `map : ('a -> 'b) -> 'a list -> 'b list`.

   - `reverse : 'a list -> 'a list`

   - `nth : 'a list * int -> 'a option`.
*)

(* ------------------------------------------------------------------------------------------------- *)
(* 
(* val partition = fn : ('a -> bool) -> 'a list -> 'a list * 'a list *)

fun partition _    []        = ([], [])
  | partition pred (x :: xs) = let
                                   val (us, vs) = partition pred xs
                               in
                                   if pred x then
                                       (x :: us, v)
                                   else
                                       (us, x :: vs)
                               end;
*)
fun partition pred xs = let
                            fun sfun (x, (us, vs)) = if pred x then
                                                         (x :: us, vs)
                                                     else
                                                         (us, x :: vs)
                        in
                            foldr sfun ([], []) xs
                        end;

(*
fun partition pred xs = let
                            fun sfun (x, (us, vs)) = if pred x then
                                                         (us @ [x], vs)
                                                     else
                                                         (us, vs @ [x])
                        in
                            foldl sfun ([], []) xs
                        end;
*)
(* ------------------------------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------------------------------- *)
(*
(* val map = fn : ('a -> 'b) -> 'a list -> 'b list *)

fun map _ []        = []
  | map f (x :: xs) = (f x) :: (map f xs);
*)
fun map f xs = let
                   fun sfun (x, y) = f x :: y
               in
                   foldr sfun [] xs
               end;

(*
fun map f xs = let
                   fun sfun (x, y) = y @ [f x]
               in
                   foldl sfun [] xs
               end;
*)
(* ------------------------------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------------------------------- *)
(*
(* val rev = fn : 'a list -> 'a list *)

fun rev []        = []
  | rev (x :: xs) = (rev xs) @ [x];

*)
fun rev xs = let
                 fun sfun (x, y) = x :: y
             in
                 foldl sfun [] xs
             end;

(*
fun rev xs = let
                 fun sfun (x, y) = y @ [x]
             in
                 foldr sfun [] xs
             end;
*)
(* ------------------------------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------------------------------- *)
datatype 'a option = SOME of 'a | NONE;

datatype 'a Find = LookingFor of int
                 | Found      of 'a;
(* 
(* val nth = fn : 'a list * int -> 'a option *)
fun nth ([], _)      = NONE
  | nth (x :: xs, y) = if y = 0 then
                           SOME x
                       else
                           nth (xs, y - 1);

(* val nthAux = fn : 'a list * int -> 'a Find *)
fun nthAux ([], y)       = LookingFor y
  | nthAux (x :: xs, y)  = if y = 0 then
                               Found x
                           else
                               nthAux (xs, y - 1);

*)
fun nthAux (x, y) = let
                        fun sfun (u, Found v)      = Found v
                          | sfun (u, LookingFor v) = if v = 0 then
                                                         Found u
                                                     else
                                                         LookingFor (v - 1)
                    in
                        foldl sfun (LookingFor y) x
                    end;

fun nth (x, y) = case (nthAux (x, y)) of
                     LookingFor _ => NONE
                   | Found v      => SOME v;
(* ------------------------------------------------------------------------------------------------- *)
(* =============================================================================================== *)

(*
## Hints.

1. All functions will be obtained by doing `foldl sfun s0 lst` for some
   appropriate summarising function. To find out what `sfun` is first
   determine what the type `'summary` should be.

2. Be careful with `reverse` as the obvious version of reverse will
   have running time O(n²). Instead get an O(n) version (Hint: For
   this case `foldl` might be more convenient).

3. For the `nth` function, first define an auxiliary datatype as follows

   ```
   datatype 'a Find = LookingFor of int
                    | Found      of 'a


   ```
   which will act as the summary. The value `LookingFor n` means that
   we are yet to find the value and it is 'n' positions away from where
   we are whereas `Found` means that we have already found it.

   Try writing `nthAux : 'a list * int -> 'a Find`
*)

(* =============================================================================================== *)
(*
# Bonus question (ungraded)

Write `foldl` in terms of `foldr` and `map`. To solve this problem
consider the summarising function `sfun` that assigns to each element
`x : 'a` a transformation `trₓ : 'summary -> summary` defined as `trₓ
s = sfun (x, s)`. Notice that `foldr sfun s0 [x0,...,xn] = (trₓ₀ o
trₓ₁ .... o trₓₙ) s0`. Now it is sufficient to compute the composition
of this function which can be done using a `foldl`
*)

(* ------------------------------------------------------------------------------------------------- *)

(*  Idea building

foldl ⊗ s0 [x₁,......, xₙ] =   ((s0 ⊗ x₁) ⊗ x₂ ...... ⊗ xₙ)

For each x : 'a consider trₓ : 'summary -> 'summary, trₓ s = s ⊗ x

So { trₓ : x ∈ 'a } gives a family of functions.

foldl ⊗ s0 [ x₁,....,xₙ] =      trₓₙ ... trₓ₂ (trₓ₁ s0)
                         =     (trₓₙ o ... o trₓ₁) s0

       [x₁,....,xₙ] -> [trₓ₁,....,trₓₙ]   map (fn x => fn s => sfun (x, s) )

sfun' (t, old) = old o t
foldr sfun' [trₓ₁, ..., trₓₙ]

Initial summary will be identity function

               id
              [tr₁]           =  tr₁ o id = tr₁
              [tr₁, tr₂]      =  tr₂ o tr₁
              [tr₁, tr₂, tr₃] =  tr₃ o tr₂ o tr₁

              old = trₙ o ... o trₙ₋ᵢ
*)

(* val foldl = fn : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b *)
fun foldl sfun s0 xs = let
                            fun sfun' (t, old) = old o t
                            fun tr x = fn s => sfun (x, s)
                            val transform = map tr
                       in
                            (foldr sfun' (fn x => x) (transform xs)) s0 
                       end;

(* Other way to write, takes list first and then the initial summary *)
(* val foldl = fn : ('a * 'b -> 'b) -> 'a list -> 'b -> 'b *)
(*
fun foldl sfun = let
                    fun sfun' (t, old) = old o t
                    fun tr x = fn s => sfun (x, s)
                    val transform = map tr
                 in
                    foldr sfun' (fn x => x) o transform
                 end;
*)

(* val foo = foldl (op ::) [] [1, 2, 3] (* Testing *) *)

(* ------------------------------------------------------------------------------------------------- *)

(* ------------------------------------------------------------------------------------------------- *)

(*  Idea building

foldl ⊗ s0 [x₁,......, xₙ] =   ((s0 ⊗ x₁) ⊗ x₂ ...... ⊗ xₙ)
foldr ⛒ [x₁, ... xₙ] s0 = x₁ ⛒ (... xₙ₋₁ ⊗ (xₙ ⛒ s0))

For each x : 'a consider trₓ : 'summary -> 'summary, trₓ s = x ⊗ s

So { trₓ : x ∈ 'a } gives a family of functions.

foldl ⊗ s0 [ x₁,....,xₙ] =      trₓₙ ... trₓ₂ (trₓ₁ s0)
                         =     (trₓₙ o ... o trₓ₁) s0

foldr ⊗ s0 [ x₁,....,xₙ] =      trₓ₁ ... trₓ₂ (trₓₙ s0)
                         =     (trₓ₁ o ... o trₓₙ) s0

       [x₁,....,xₙ] -> [trₓ₁,....,trₓₙ]   map (fn x => fn s => sfun (x, s) )

sfun' (t, old) = old o t
foldl sfun' [trₓ₁, ..., trₓₙ]

Initial summary will be identity function
*)

(* val foldr = fn : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b *)
fun foldr sfun s0 xs =  let
                            fun sfun' (t, old) = old o t
                            fun tr x = fn s => sfun (x, s)
                            val transform = map tr
                        in
                            (foldl sfun' (fn x => x) (transform xs)) s0
                        end;

(* Other way to write, takes list first and then the initial summary *)
(* val foldr = fn : ('a * 'b -> 'b) -> 'a list -> 'b -> 'b *)
(*
fun foldr sfun = let
                    fun sfun' (t, old) = old o t
                    fun tr x = fn s => sfun (x, s)
                    val transform = map tr
                 in
                    foldl sfun' (fn x => x) o transform
                 end;
*)
(* ------------------------------------------------------------------------------------------------- *)
(* =============================================================================================== *)