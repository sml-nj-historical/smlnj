(* gsched.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

signature GLOBAL_SCHED = sig
  structure F : FLOWGRAPH

  val gsched : F.block list -> unit

end

functor GlobalSched(structure Flowgraph : FLOWGRAPH
			  val codegen : Flowgraph.cluster -> unit) =
struct
    fun gsched cluster = codegen cluster
end


(*
 * $Log$
 *)
