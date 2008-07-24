(* ordset-sig.sml
 *
 * COPYRIGHT (c) 1993 by AT&T Bell Laboratories.  See COPYRIGHT file for details.
 *
 * Signature for a set of values with an order relation.
 *)

(* DBM: modified version for srcinfo, with added functionality: least, greatest, next, previous *)


signature ORD_SET =
  sig

    structure Key : ORD_KEY

    type item = Key.ord_key
    type set

    val empty : set
	(* The empty set *)

    val singleton : item -> set
	(* Create a singleton set *)

    val fromList : item list -> set
	(* create a set from a list of items *)

    val add  : set * item -> set
    val add' : item * set -> set
	(* Insert an item. *)

    val addList : set * item list -> set
	(* Insert items from list. *)

    val delete : set * item -> set
	(* Remove an item. Raise NotFound if not found. *)

    val member : set * item -> bool
	(* Return true if and only if item is an element in the set *)

    val least : set -> item option
        (* Return the least item in the set, if one exists *)

    val greatest : set -> item option
        (* Return the greatest item in the set, if one exists *)

    val nextOrEqual : set * item -> item option
        (* Return the least item in the set greater or equal to the item, if one exists.
	   The item need not be a member of the set. *)

    val next : set * item -> item option
        (* Return the least item in the set greater than the item, if one exists.
	   The item need not be a member of the set. *)

    val previous : set * item -> item option
        (* Return the greatest item in the set less than the item, if one exists.
	   The item need not be a member of the set. *)

    val isEmpty : set -> bool
	(* Return true if and only if the set is empty *)

    val equal : (set * set) -> bool
	(* Return true if and only if the two sets are equal *)

    val compare : (set * set) -> order
	(* does a lexical comparison of two sets *)

    val isSubset : (set * set) -> bool
	(* Return true if and only if the first set is a subset of the second *)

    val numItems : set ->  int
	(* Return the number of items in the set *)

    val listItems : set -> item list
	(* Return an ordered list of the items in the set *)

    val union : set * set -> set
        (* Union *)

    val intersection : set * set -> set
        (* Intersection *)

    val difference : set * set -> set
        (* Difference *)

    val map : (item -> item) -> set -> set
	(* Create a new set by applying a map function to the elements
	 * of the set.
         *)
     
    val app : (item -> unit) -> set -> unit
	(* Apply a function to the entries of the set 
         * in increasing order
         *)

    val foldl : (item * 'b -> 'b) -> 'b -> set -> 'b
	(* Apply a folding function to the entries of the set 
         * in increasing order
         *)

    val foldr : (item * 'b -> 'b) -> 'b -> set -> 'b
	(* Apply a folding function to the entries of the set 
         * in decreasing order
         *)

    val partition : (item -> bool) -> set -> (set * set)

    val filter : (item -> bool) -> set -> set

    val exists : (item -> bool) -> set -> bool

    val find : (item -> bool) -> set -> item option

  end (* ORD_SET *)
