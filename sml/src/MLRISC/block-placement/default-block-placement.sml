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

    fun blocks () = let
      val entryBlk = (ENTRY, #node_info graph ENTRY)
      val exitBlk = (EXIT, #node_info graph EXIT)
      fun filter([]) = [exitBlk]
	| filter((node as (i, CFG.BLOCK{kind, ...}))::rest) = 
	    (case kind
	     of CFG.START  => filter rest
	      | CFG.STOP   => filter rest
	      | CFG.NORMAL => node::filter rest
	    (*esac*))
    in entryBlk :: filter(#nodes graph ())
    end
  in 
      (cfg, blocks())
  end 
end
