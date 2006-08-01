(* Copyright 1989 by AT&T Bell Laboratories *)

signature PPUTILNEW =
sig

  datatype break_style = CONSISTENT | INCONSISTENT

  val openStyleBox : break_style -> PrettyPrintNew.stream -> PrettyPrintNew.indent -> unit

  val ppSequence : PrettyPrintNew.stream ->
		   {sep: PrettyPrintNew.stream->unit, 
		    pr: PrettyPrintNew.stream->'a->unit,
		    style: break_style}
		   -> 'a list -> unit
  val ppClosedSequence : PrettyPrintNew.stream
			 -> {front:PrettyPrintNew.stream->unit, 
                             sep:PrettyPrintNew.stream->unit,
			     back:PrettyPrintNew.stream->unit,
                             pr:PrettyPrintNew.stream->'a->unit,
			     style:break_style}
			 -> 'a list -> unit
  val ppSym : PrettyPrintNew.stream -> Symbol.symbol -> unit
  val mlstr : string -> string
  val pp_mlstr : PrettyPrintNew.stream -> string -> unit
  val pp_intinf : PrettyPrintNew.stream -> IntInf.int -> unit
  val ppvseq : PrettyPrintNew.stream
               -> int -> string -> (PrettyPrintNew.stream -> 'a -> unit)
               -> 'a list -> unit
  val ppvlist : PrettyPrintNew.stream
               -> string * string * (PrettyPrintNew.stream -> 'a -> unit) * 'a list
               -> unit
  val ppvlist' : PrettyPrintNew.stream
               -> string * string * (PrettyPrintNew.stream -> string -> 'a -> unit)
                    * 'a list
               -> unit
  val ppIntPath : PrettyPrintNew.stream -> int list -> unit
  val ppSymPath : PrettyPrintNew.stream -> SymPath.path -> unit
  val ppInvPath : PrettyPrintNew.stream -> InvPath.path -> unit
  val nl_indent : PrettyPrintNew.stream -> int -> unit

  (* needed in PPTypes, PPModules *)
  val findPath : InvPath.path * ('a -> bool) * (SymPath.path -> 'a)
                 -> (Symbol.symbol list * bool)

  val ppTuple: PrettyPrintNew.stream
	       -> (PrettyPrintNew.stream -> 'a -> unit) -> 'a list -> unit

  val ppi: PrettyPrintNew.stream -> int -> unit
  val ppcomma : PrettyPrintNew.stream -> unit
  val ppcomma_nl : PrettyPrintNew.stream -> unit
  val nl_app : PrettyPrintNew.stream -> (PrettyPrintNew.stream -> 'a -> unit)
               -> 'a list -> unit 
  val br_app : PrettyPrintNew.stream -> (PrettyPrintNew.stream -> 'a -> unit)
               -> 'a list -> unit 
  val en_pp : PrettyPrintNew.stream -> 
              {break      : {nsp: int, offset: int} -> unit, 
	       newline    : unit -> unit,
	       openHVBox  : int -> unit,
	       openHOVBox : int -> unit,
	       closeBox   : unit -> unit, 
	       pps        : string -> unit}
  val ppArray : PrettyPrintNew.stream -> 
                (PrettyPrintNew.stream -> 'a -> unit) * 'a array
	        -> unit

end (* signature PPUTILNEW *)
