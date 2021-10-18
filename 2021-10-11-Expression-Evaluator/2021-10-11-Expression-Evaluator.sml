(* # Expression evaluator *)

(*
1. Design an algebraic data type to capture expressions over real constants. You can
   have operators + and *.
*)

datatype Expr = Const of real
			  | Var   of string
			  | Plus  of Expr * Expr
			  | Mul   of Expr * Expr

(*
2. Design a small programming language which consists of two things

	- An assignement of variables x := e
	- Printing an expresions value print e

	```sml

	datatype Expr = ...

	(* Expressions can either be

	     1. Constant
		 2. Variable
		 3. Plus applied on two expressions
		 4. Mul applied on two expressions

	*)
	datatype Stmt = ...

    (*
	   1. Assignment
	   2. Print statement

	*)

	```

	You capture the abstract syntax of the language as an algebraic data type in SML.

	- Evaluator : evaluates the given expression with respect to the current binding of variables.
	- Executor  : Executes the current statement (thereby modifying the bindings) to give a new
		binding environment.
*)

datatype Stmt = Assign of string * Expr
			  | Print  of Expr

(*
3. Use a the Map data structure as the environment for variable bindings and design
   your expression evaluator and statement executor
*)

type EnvMap = real AtomMap.map

(* Expression evaluator *)
(* evalMap : Expr -> EnvMap -> real option *)
fun evalMap (Const x)     _   = SOME x
  | evalMap (Var x)       env = AtomMap.find (env, Atom.atom x)
  | evalMap (Plus (x, y)) env =
		let
			val res = case (evalMap x env) of
						NONE        => NONE
					  | (SOME xres) => case (evalMap y env) of
					  					NONE        => NONE
									  | (SOME yres) => SOME (xres + yres)
		in
			res
		end
  | evalMap (Mul (x, y)) env =
		let
			val res = case (evalMap x env) of
						NONE        => NONE
					  | (SOME xres) => case (evalMap y env) of
					  					NONE        => NONE
									  | (SOME yres) => SOME (xres * yres)
		in
			res
		end

(* execMap : Stmt -> EnvMap -> EnvMap *)
fun execMap (Assign (x, y)) env =
		let
		  val res = case (evalMap y env) of
						NONE        => env
					  | (SOME yres) => AtomMap.insert (env, Atom.atom x, yres)
		in
		  res
		end
  | execMap (Print x)       env = 
		let
		  val res = case (evalMap x env) of
						NONE		=> ((print "NONE\n"); env)
					  | (SOME xres) => ((print (Real.toString xres)); (print "\n"); env)
		in
		  res
		end

(*	TEST CODE *)
(*
(* ------------------------------------------------------------------------------------------------- *)
val envMap : EnvMap = AtomMap.empty

(* Swapping of Variables *)
val envMap = execMap (Assign ("a", Plus (Const 5.0, Mul (Const 5.0, Const 1.0)))) envMap  (* a := 5 + (5 * 1) *)
val envMap = execMap (Assign ("b", Mul (Plus (Const 5.0, Const 5.0), Var "a"))) envMap	(* b := (5 + 5) * a *)
val envMap = execMap (Assign ("c", Var "b")) envMap	(* c := b *)
val envMap = execMap (Assign ("b", Var "a")) envMap	(* b := a *)
val envMap = execMap (Assign ("a", Var "c")) envMap  (* a := c *)
val envMap = execMap (Print (Var "a")) envMap
val envMap = execMap (Print (Var "b")) envMap
val envMap = execMap (Print (Plus (Var "a", Var "b"))) envMap
val envMap = execMap (Print (Var "key")) envMap
(* ------------------------------------------------------------------------------------------------- *)
*)

(*
4. Alternatively use HashTable data structure to maintain the variable bindings and execute
   statements.
*)

type EnvHash = real AtomTable.hash_table

(* Expression evaluator *)
(* evalHash : Expr -> EnvHash -> real option *)
fun evalHash (Const x)     _   = SOME x
  | evalHash (Var x)       env = AtomTable.find env (Atom.atom x)
  | evalHash (Plus (x, y)) env =
		let
			val res = case (evalHash x env) of
						NONE        => NONE
					  | (SOME xres) => case (evalHash y env) of
					  					NONE        => NONE
									  | (SOME yres) => SOME (xres + yres)
		in
			res
		end
  | evalHash (Mul (x, y)) env =
		let
			val res = case (evalHash x env) of
						NONE        => NONE
					  | (SOME xres) => case (evalHash y env) of
					  					NONE        => NONE
									  | (SOME yres) => SOME (xres * yres)
		in
			res
		end

(* execHash : Stmt -> EnvHash -> () *)
fun execHash (Assign (x, y)) env =
		let
		  val res = case (evalHash y env) of
						NONE        => ()
					  | (SOME yres) => AtomTable.insert env (Atom.atom x, yres)
		in
		  res
		end
  | execHash (Print x)       env = 
		let
		  val res = case (evalHash x env) of
						NONE		=> (print "NONE\n")
					  | (SOME xres) => ((print (Real.toString xres)); (print "\n"))
		in
		  res
		end

(*	TEST CODE *)
(*
(* ------------------------------------------------------------------------------------------------- *)
exception KeyNotFound

val envHash : EnvHash = AtomTable.mkTable (10, KeyNotFound)

(* Swapping of Variables *)
val _ = execHash (Assign ("a", Plus (Const 5.0, Mul (Const 5.0, Const 1.0)))) envHash  (* a := 5 + (5 * 1) *)
val _ = execHash (Assign ("b", Mul (Plus (Const 5.0, Const 5.0), Var "a"))) envHash	   (* b := (5 + 5) * a *)
val _ = execHash (Assign ("c", Var "b")) envHash	(* c := b *)
val _ = execHash (Assign ("b", Var "a")) envHash	(* b := a *)
val _ = execHash (Assign ("a", Var "c")) envHash  (* a := c *)
val _ = execHash (Print (Var "a")) envHash
val _ = execHash (Print (Var "b")) envHash
val _ = execHash (Print (Plus (Var "a", Var "b"))) envHash
val _ = execHash (Print (Var "key")) envHash
(* ------------------------------------------------------------------------------------------------- *)
*)

(*
For a detailed documentation of the Map and the HashTable data
structure see the documentation at.

<https://www.classes.cs.uchicago.edu/archive/2015/spring/22620-1/smlnj-lib.html>
*)