(* bug428.sml *)

signature WR = sig

type Wr

val to_stdout: unit -> Wr
val to_file: string -> Wr
val to_nowhere: unit -> Wr
val to_wrs: Wr list -> Wr
val to_fn: (string->unit) -> (unit->unit) -> Wr
val to_string: unit -> Wr
val extract_str: Wr -> string

val close: Wr -> unit

val write_wr: Wr -> string -> unit

end;
signature PP = sig

structure Wr: WR

type Pp

val pp_from_wr: Wr.Wr -> Pp
val wr_from_pp: Pp -> Wr.Wr;

val pwrite : Pp -> string -> unit
val setb: Pp -> unit
val endb: Pp -> unit
val break: Pp -> bool -> int -> unit
val expbreak: Pp -> bool -> string -> unit
val set_margin: Pp -> int -> unit

val DEBUG: bool ref

end;

signature WRMGT = sig

(* Maintains a notion of a current (prettyprinting) writer 
   and its associated prettyprinter *)

structure Wr: WR;
structure Pp: PP;
sharing Pp.Wr = Wr;

val set_current_wr: Wr.Wr -> unit;
val get_current_wr: unit -> Wr.Wr;
val stdpp: unit -> Pp.Pp;

val write: string -> unit;

end;

signature STRINGUTILS = sig

end;

signature REGISTRY = sig

type registeredtype

val register: string -> (registeredtype->unit) -> unit
val registerflag: string -> (registeredtype ref) -> unit

val set_flag: string -> registeredtype -> unit
val set_all: registeredtype -> unit

end;

signature LISTUTILS = sig

val memq: ('a -> 'a -> bool) -> 'a list -> 'a -> bool

val mapappend: ('a -> 'b list) -> ('a list) -> ('b list)
val mapunit: ('a -> 'b) -> ('a list) -> unit
val mapunit_tuple: ('a -> unit) -> (unit -> unit) -> ('a list) -> unit

val mapfold: ('a -> 'b) -> ('b -> 'b -> 'b) -> 'b -> ('a list) -> 'b
val forall: ('a -> bool) -> ('a list) -> bool
val forsome: ('a -> bool) -> ('a list) -> bool

val filter: ('a -> bool) -> ('a list) -> ('a list)

end;

signature ID = sig

type T 

val intern: string -> T
val tostr: T -> string

val hashcode: T -> int 
val new: unit -> T
val new_from: T -> T

val == : T -> T -> bool

end;


(* May eventually want to support these too:

   val lexlt : T -> T -> bool
*)

signature DEBUGUTILS = sig

val wrap: (bool ref) -> string -> (unit -> 'a) -> (unit -> unit) -> ('a -> unit) -> 'a

end;

signature GLOBALS = sig

structure Wr: WR
structure Pp: PP
structure WrMgt: WRMGT
structure Id: ID

sharing Pp.Wr = Wr
sharing WrMgt.Pp = Pp

include WRMGT

include LISTUTILS
include STRINGUTILS
include DEBUGUTILS
include REGISTRY

sharing type registeredtype = bool

end;

signature TYPPVT = sig

structure Globals: GLOBALS
open Globals

datatype pretyp = 
	    PRETVAR of Id.T
	  | PREARROW of pretyp * pretyp
	  | PREALL of Id.T * pretyp * pretyp
	  | PREMEET of pretyp list

datatype T = 
	    TVAR of unit * int
	  | ARROW of unit * T * T
	  | ALL of {name:Id.T} * T * T
	  | MEET of unit * (T list)
	  
datatype tenvelt = BND of Id.T * T
		 | ABB of Id.T * T
		 | VBND of Id.T * T

datatype tenv = TENV of tenvelt list

val empty_tenv: tenv
val extend_bound: tenv -> Id.T -> T -> tenv
val push_bound: tenv -> Id.T -> T -> tenv
val extend_abbrev: tenv -> Id.T -> T -> tenv
val push_abbrev: tenv -> Id.T -> T -> tenv
val extend_binding: tenv -> Id.T -> T -> tenv
val push_binding: tenv -> Id.T -> T -> tenv
val pop: tenv -> tenv

val index: tenv -> Id.T -> int
val lookup_name: tenv -> int -> Id.T
val lookup_and_relocate_bound: tenv -> int -> T
val lookup_and_relocate_binding: tenv -> int -> T
val lookup_and_relocate: tenv -> int -> tenvelt
val lookup: tenv -> int -> tenvelt
val relocate: int -> T -> T

exception UnknownId of string
exception WrongKindOfId of tenv * int * string
val debruijnify: tenv -> pretyp -> T

val prt: Pp.Pp -> tenv -> T -> unit
val prt_tenv: Pp.Pp -> tenv -> unit

val NS: T

end;
