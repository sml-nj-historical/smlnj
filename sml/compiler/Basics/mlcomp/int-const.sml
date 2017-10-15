(* int-const.sml
 *
 * A common representation of integer literals to use throughout the different
 * intermediate representations (from Absyn to CPS).
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *)

structure IntConst : sig

    type 'ty iconst = {
	value : IntInf.int,	(* value *)
	ty : 'ty		(* the "type" of the value *)
      }

    val toString : 'ty iconst -> string

    val fmt : ('ty -> string) -> 'ty iconst -> string

  (* do two constants have equal values? *)
    val same : 'ty iconst * 'ty iconst -> bool

  end = struct

    type 'ty iconst = {
	value : IntInf.int,	(* value *)
	ty : 'ty		(* the "type" of the value *)
      }

    fun toString {value, ty} = IntInf.toString value

    fun fmt tyToString {value, ty} = concat[IntInf.toString value, ":", tyToString ty]

    fun same (a : 'ty iconst, b : 'ty iconst) = (#value a = #value b)

  end
