(* Copyright 1989 by AT&T Bell Laboratories *)

signature PPUTIL =
sig
  val ppSequence : PrettyPrint.ppstream ->
		   {sep: PrettyPrint.ppstream->unit, 
		    pr: PrettyPrint.ppstream->'a->unit,
		    style: PrettyPrint.break_style}
		   -> 'a list -> unit
  val ppClosedSequence : PrettyPrint.ppstream
			 -> {front:PrettyPrint.ppstream->unit, 
                             sep:PrettyPrint.ppstream->unit,
			     back:PrettyPrint.ppstream->unit,
                             pr:PrettyPrint.ppstream->'a->unit,
			     style:PrettyPrint.break_style}
			 -> 'a list -> unit
  val ppSym : PrettyPrint.ppstream -> Symbol.symbol -> unit
  val mlstr : string -> string
  val pp_mlstr : PrettyPrint.ppstream -> string -> unit
  val ppvseq : PrettyPrint.ppstream
               -> int -> string -> (PrettyPrint.ppstream -> 'a -> unit)
               -> 'a list -> unit
  val ppvlist : PrettyPrint.ppstream
               -> string * string * (PrettyPrint.ppstream -> 'a -> unit) * 'a list
               -> unit
  val ppIntPath : PrettyPrint.ppstream -> int list -> unit
  val ppSymPath : PrettyPrint.ppstream -> SymPath.path -> unit
  val ppInvPath : PrettyPrint.ppstream -> InvPath.path -> unit
  val nl_indent : PrettyPrint.ppstream -> int -> unit

  (* needed in PPTypes, PPModules *)
  val findPath : InvPath.path * ('a -> bool) * (SymPath.path -> 'a)
                 -> (Symbol.symbol list * bool)

  val ppTuple: PrettyPrint.ppstream
	       -> (PrettyPrint.ppstream -> 'a -> unit) -> 'a list -> unit

  val ppi: PrettyPrint.ppstream -> int -> unit
  val add_comma : PrettyPrint.ppstream -> unit
  val add_comma_nl : PrettyPrint.ppstream -> unit
  val nl_app : PrettyPrint.ppstream -> (PrettyPrint.ppstream -> 'a -> unit)
               -> 'a list -> unit 
  val br_app : PrettyPrint.ppstream -> (PrettyPrint.ppstream -> 'a -> unit)
               -> 'a list -> unit 
  val en_pp : PrettyPrint.ppstream -> 
              {add_break   : int * int -> unit, 
	       add_newline : unit -> unit,
	       begin_block : PrettyPrint.break_style -> int -> unit,
	       end_block   : unit -> unit, 
	       pps : string -> unit}
  val ppArray : PrettyPrint.ppstream -> 
                (PrettyPrint.ppstream -> 'a -> unit) * 'a array
	        -> unit
end (* signature PPUTIL *)


