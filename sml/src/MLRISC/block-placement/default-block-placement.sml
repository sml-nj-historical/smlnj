(* default-block-placement.sml
 *
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *)

(* Just the order in which blocks were generated *)
functor DefaultBlockPlacement (CFG : CONTROL_FLOW_GRAPH) : BLOCK_PLACEMENT = 
struct
  structure CFG=CFG
  structure G = Graph

  fun error msg = MLRiscErrorMsg.error ("NaiveBlockPlacement", msg)

  fun blockPlacement (cfg as G.GRAPH graph) = let
    val ENTRY = 
      (case #entries graph () of [n] => n | _ => error "ENTRY")
    val EXIT = 
      (case #exits graph () of [n] => n | _ => error "EXIT")

  in 
      List.filter
         (fn (i, CFG.BLOCK{kind, ...}) => i <> ENTRY andalso i <> EXIT)
	 (#nodes graph ())
  end 
end
