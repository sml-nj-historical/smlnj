(* mono-array.sig
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * Generic interface for monomorphic array structures.
 *
 *)

signature MONO_ARRAY =
  sig

    eqtype array
    type elem
    type vector

    val maxLen : int

  (* array creation functions *)
    val array    : (int * elem) -> array
    val tabulate : (int * (int -> elem)) -> array
    val fromList : elem list -> array

    val length   : array -> int
    val sub      : (array * int) -> elem
    val update   : (array * int * elem) -> unit
    val extract  : (array * int * int option) -> vector

    val copy : {
	    src : array, si : int, len : int option,
	    dst : array, di : int
	  } -> unit
    val copyVec : {
	    src : vector, si : int, len : int option,
	    dst : array, di : int
	  } -> unit

    val app    : (elem -> unit) -> array -> unit
    val foldl  : ((elem * 'a) -> 'a) -> 'a -> array -> 'a
    val foldr  : ((elem * 'a) -> 'a) -> 'a -> array -> 'a
    val modify : (elem -> elem) -> array -> unit

    val appi    :
	  ((int * elem) -> unit) -> (array * int * int option) -> unit
    val foldli  :
	  ((int * elem * 'a) -> 'a) -> 'a -> (array * int * int option) -> 'a
    val foldri  :
	  ((int * elem * 'a) -> 'a) -> 'a -> (array * int * int option) -> 'a
    val modifyi :
	  ((int * elem) -> elem) -> (array * int * int option) -> unit

  end

