(* control-reps.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *)

structure ControlReps =
  struct

  (* priorities are used for ordering help messages (lexical order) *)
    type priority = int list

    datatype 'a control = Ctl of {
	name : Atom.atom,		(* name of the control *)
	set : 'a -> unit,		(* function to set the control's value *)
	get : unit -> 'a,		(* return the control's value *)
	priority : priority,		(* control's priority *)
	obscurity : int,		(* control's detail level; higher means *)
					(* more obscure *)
	help : string			(* control's description *)
      }

    withtype ('a, 'b) control_set =
	  {ctl : 'a control, info : 'b} AtomTable.hash_table

  (* conversion functions for control values *)
    type 'a value_cvt = {
	tyName : string,
	fromString : string -> 'a option,
	toString : 'a -> string
      }

  (* ">" ordering on priorities *)
    fun priorityGT ([], _) = false
      | priorityGT (_, []) = true
      | priorityGT (x::xs, y::ys) =
	  Int.>(x, y) orelse ((x = y) andalso priorityGT(xs, ys))

  end
