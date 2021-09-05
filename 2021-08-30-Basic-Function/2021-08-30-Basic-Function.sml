(*
    1. Write the tri-variate (i.e. 3 variable) versions of `curry` and
       `uncurry` functions. First step is to write down the type (in the
       comments).
*)

    (*
       Type of curry:
       val curry = fn : ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
    *)
    (* Method: 1 *) fun curry f x y z = f (x, y, z);
    (* Method: 2 *) val curry = fn f => fn x => fn y => fn z => f (x, y, z);

    (*
       Type of uncurry:
       val uncurry = fn : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd)
    *)
    (* Method: 1 *) fun uncurry f (x, y, z) = f x y z;
    (* Method: 2 *) val uncurry = fn f => fn (x, y, z) => f x y z;


(*
    2. Write the functions `fst : 'a * 'b -> 'a` and `snd : 'a * 'b -> 'b`
       that project a tuple into its components.
*)

    (* val fst = fn : 'a * 'b -> 'a *)
    fun fst (a, b) = a;

    (* val snd = fn : 'a * 'b -> 'b *)
    fun snd (a, b) = b;


(* 3. Write the length function for lists `length : 'a list -> int`. *)

    (* val length = fn : 'a list -> int *)
    fun length []        = 0
      | length (x :: xs) = 1 + (length xs);


(*
    4. Write the `reverse : 'a list -> 'a list` function. Be careful to
       not make it O(n^2)
*)

    (* val reverse = fn : 'a list -> 'a list *)
    fun reverse []        = []
      | reverse (x :: xs) = (reverse xs) @ (x :: []);


(*
    5. Write a function to compute the nth element in the Fibonacci
       sequence `fib : int -> int`. Be careful for the obvious version is
       exponential.
*)

    (* Assuming Fibonacci series starts with 1, 1, ... *)

    (* val fibHelper = fn : int -> int -> int -> int *)
    fun fibHelper 1 a b = a
      | fibHelper n a b = fibHelper (n - 1) b (a + b);

    (* val fib = fn : int -> int *)
    fun fib n = fibHelper n 1 1;