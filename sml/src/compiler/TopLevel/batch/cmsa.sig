(* COPYRIGHT (c) 1997 Bell Labs, Lucent Technologies *)
(* cmsa.sig *)

signature CMSA = sig

    type env				(* environments *)
    type sym				(* symbols *)

    (* build symbols from strings *)
    val STR: string -> sym		(* structure *)
    val SIG: string -> sym		(* signature *)
    val FCT: string -> sym		(* functor *)
    val FSIG: string -> sym		(* funsig *)

    val pervenv: unit -> env		(* fetch pervasive environment *)
    val register: env -> unit		(* register delta with toplevel env. *)

    (* layer environments, head of list goes on top *)
    val layer: env list -> env

    (* filter environment by list of symbols *)
    val filter: sym list -> env -> env

    (* load or compile (1st arg), then execute *)
    val run: string * env -> env

end (* signature CMSA *)


(*
 * $Log$
 *)
