(*
1.  For lists define the function `map : ('a -> 'b) -> 'a list -> 'b list`. The semantics of map is that it applies the given function
    on all the elements of the list, i.e.

         map f [x1, x2,....,xn] = [f x1, f x2, ... f xn]
*)

    (*
        val map = fn : ('a -> 'b) -> 'a list -> 'b list
    *)
    fun map f []        = []
      | map f (x :: xs) = (f x) :: (map f xs);


(* 2.  Define the data type `'a tree` that captures a binary tree. *)

    datatype 'a tree = null | Node of 'a tree * 'a * 'a tree;
    (*
        datatype 'a tree = Node of 'a tree * 'a * 'a tree | null
    *)


(*
3.  Can you write a function `treemap` analogues to `map` for list ?
    First write its type and then complete its definition.
*)

    (*
        val treemap = fn : ('a -> 'b) -> 'a tree -> 'b tree
    *)
    fun treemap f null             = null
      | treemap f (Node (l, x, r)) = Node (treemap f l, f x, treemap f r);


(*
4.  Define the in-order, pre-order and post-order traversal of the
    binary tree returning the list of nodes in the given order. First
    write down the type of the function(s) and then go about defining
    them.
*)

    (* val inorder = fn : 'a tree -> 'a list *)
    fun inorder null             = []
      | inorder (Node (l, x, r)) = (inorder l) @ [x] @ (inorder r);

    (* val preorder = fn : 'a tree -> 'a list *)
    fun preorder null             = []
      | preorder (Node (l, x, r)) = [x] @ (preorder l) @ (preorder r);

    (* val postorder = fn : 'a tree -> 'a list *)
    fun postorder null             = []
      | postorder (Node (l, x, r)) = (postorder l) @ (postorder r) @ [x];


(*
5.  Define the rotate clockwise function for binary trees. Pictorially
    this rotation function is defined as the following.

                    a                   b
                   / \                 / \
                  / ⭮ \               /   \
                 b    🌲₃  ======>   🌲₁   a
                / \                       / \
               /   \                     /   \
              🌲₁   🌲₂                 🌲₂    🌲₃

    If the left subtree of the root is null then rotation is identity operation.
*)

    (* val rotate = fn : 'a tree -> 'a tree *)
    fun rotate null                            = null
      | rotate (Node (null, x, r))             = Node (null, x, r)
      | rotate (Node (Node (l, xs, rs), x, r)) = Node (l, xs, Node (rs, x, r));
