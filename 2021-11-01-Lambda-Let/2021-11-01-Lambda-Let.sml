(*
# Lambda Calculus with let and let-rec

Deadline : 9th November 23:59

Consider variants of lambda calculus λ-calculus ⊆ λ-let ⊆ λ-letrec
where the former adds the syntactic sugar of non-recursive `let` and
the latter adds `letrec`. It adds the construct `let x = e₁ in e₂ end`
and `letrec x = e₁ in e₂ end`. The difference in `let` and `letrec` is
that the former is a non-recursive let where are the later is
recursive let.

 - In nonrecursive let `let x = e₁ in e₂` any occurance of x in e₁ is those
   that are bound outside the let expression itself

   For example in the code below
   ```
   let x := 10 in               -- Line (1)
      let x := x + 1            -- Line (2)
	   in x end                 -- Line (3)
   end
   ```

   The x in Line (3) is the x that is bound in line (2) whereas the x in line 2 on the RHS of
   the binding x := x +1 is the x bound in Line (1). In particular, the program

   ```
   let x := x+1 in x end

   ```
   should give an error because the x in the RHS of the x := x+1 binding is not bound.


   - In recursive let (called letrec) `letrec x := e₁ in e₂ end` the x
     that occurs in e₁ is the one that is bound in the letrec itself.
*)



(*
1. Define abstract syntax for λ-let and λ-letrec as a SML datatype.
*)

type var = string

(* λ-calculus SML datatype *)
datatype lambda = Var of var
                | Apply of lambda * lambda
                | Lambda of var * lambda

(* λ-let SML datatype *)
datatype lambdalet = Var_let of var
                   | Apply_let of lambdalet * lambdalet
                   | Lambda_let of var * lambdalet
                   | Let of var * lambdalet * lambdalet

(* λ-letrec SML datatype *)
datatype lambdaletrec = Var_letrec of var
                      | Apply_letrec of lambdaletrec * lambdaletrec
                      | Lambda_letrec of var * lambdaletrec
                      | Letrec of var * lambdaletrec * lambdaletrec

(*
2. Write the conversion λ-let to λ-calculus.
*)

(* unlet : lambdalet -> lambda *)
fun unlet (Var_let x)          = Var x
  | unlet (Apply_let (e1, e2)) = Apply (unlet e1, unlet e2)
  | unlet (Lambda_let (x, e))  = Lambda (x, unlet e)
  | unlet (Let (x, e1, e2))    = Apply (Lambda (x, unlet e2), unlet e1)

(*
## Recursion (bonus, ungraded).

As described in the class, recursion is done in λ-calculus using a
fixed point combinator like `Y`.

1. Give a conversion from λ-letrec to λ-let and then to λ-calculus
   making use of the fixed point combinator.
*)

(* 
   Y = λ f . (λ x . f (x x)) (λ x . f (x x))
   The function Y computes the fixed point of Lambda_letrec
   Y F = (λ x . F (x x) ) (λ x . F (x x))
*)
(* Y : lambdaletrec -> lambdaletrec *)
fun Y (Lambda_letrec (x, e)) =
         let
            val F = Lambda_letrec (x, e)
            val sym = "x"
            val expr = Lambda_letrec (sym, Apply_letrec (F, Apply_letrec (Var_letrec sym, Var_letrec sym)))
         in
            Apply_letrec (expr, expr)
         end
  | Y F                      = F

(* unletrec : lambdaletrec -> lambdalet *)
fun unletrec (Var_letrec x)          = Var_let x
  | unletrec (Apply_letrec (e1, e2)) = Apply_let (unletrec e1, unletrec e2)
  | unletrec (Lambda_letrec (x, e))  = Lambda_let (x, unletrec e)
  | unletrec (Letrec (x, e1, e2))    = Let (x, unletrec (Y (Lambda_letrec (x, e1))), unletrec e2)

(*
2. How does one handle mutual recursion ? Hint: use pairing
   construction.
*)