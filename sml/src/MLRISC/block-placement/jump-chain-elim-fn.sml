(* jump-chain-elim-fn.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * Collapse jumps to jumps.
 *)

functor JumpChainElimFn (

    structure CFG : CONTROL_FLOW_GRAPH
    structure InsnProps : INSN_PROPERTIES
      where I = CFG.I

  ) : sig

    structure CFG : CONTROL_FLOW_GRAPH

    val run : (CFG.cfg * CFG.node list) -> CFG.node list

  end = struct

    structure CFG = CFG
    structure IP = InsnProps
    structure G = Graph

  (* flags *)
    val disable = MLRiscControl.getFlag "disable-jump-chain-elim"
    val dumpCFG = MLRiscControl.getFlag "dump-cfg-jump-chain-elim"
    val dumpStrm = MLRiscControl.debug_stream

    fun run (cfg, blocks) = let
	  val G.GRAPH{
		  node_info, out_edges, set_out_edges, in_edges,
		  forall_nodes, remove_node, ...
		} = cfg
	  val needFilter = ref false
	(* map a block ID to a label *)
	  fun labelOf blkId = (case node_info blkId
		 of CFG.BLOCK{labels=ref(lab::_), ...} => lab
		  | CFG.BLOCK{labels, ...} => let
		      val lab = Label.anon()
		      in
			labels := [lab];
			lab
		      end
		(* end case *))
	(* given a destination block ID, check to see if it is a block that consists
	 * a single jump instruction.  If so, return the block ID and label of the
	 * block at the end of the jump chain; otherwise return NONE.
	 *)
	  fun followChain blkId = (case node_info blkId
		 of CFG.BLOCK{insns as ref[_], kind=CFG.NORMAL, ...} => (
		    (* a normal block with one instruction *)
		      case out_edges blkId
		       of [e as (_, dst, CFG.EDGE{k=CFG.JUMP, w, a})] => (
			  (* the instruction must be a jump so transitively follow it
			   * to get the target; but be careful to avoid infinite loops.
			   *)
			    set_out_edges (blkId, []);
			    case followChain dst
			     of NONE => (
				  set_out_edges (blkId, [e]);
				  SOME(dst, labelOf dst))
			      | (someLab as SOME(dst', lab)) => (
				  insns := [IP.jump lab];
				  set_out_edges (blkId,
				    [(blkId, dst', CFG.EDGE{k=CFG.JUMP, w=w, a=a})]);
				  someLab)
			    (* end case *))
			| _ => NONE
		      (* end case *))
		  | _ => NONE
		(* end case *))
	  fun doBlock (blkId, CFG.BLOCK{insns, kind=CFG.NORMAL, ...}) = let
		fun setTargets targets = let
		      val jmp::r = !insns
		      in
			needFilter := true;
			insns := IP.setTargets(jmp, targets) :: r
		      end
		in
		  case out_edges blkId
		   of [(_, dst, info as CFG.EDGE{k=CFG.JUMP, ...})] => (
			case followChain dst
			 of SOME(dst', lab) => (
			      setTargets [lab];
			      set_out_edges (blkId, [(blkId, dst', info)]))
			  | NONE => ()
			(* end case *))
		    | [(_, dst1, info as CFG.EDGE{k=CFG.BRANCH true, ...}), e2] => (
			case followChain dst1
			 of SOME(dst', lab) => (
			      setTargets [lab, labelOf(#2 e2)];
			      set_out_edges (blkId, [(blkId, dst', info), e2]))
			  | NONE => ()
			(* end case *))
		    | [e1, (_, dst2, info as CFG.EDGE{k=CFG.BRANCH true, ...})] => (
			case followChain dst2
			 of SOME(dst', lab) => (
			      setTargets [lab, labelOf(#2 e1)];
			      set_out_edges (blkId, [e1, (blkId, dst', info)]))
			  | NONE => ()
			(* end case *))
(* FIXME: do something about jump tables *)
		    | _ => ()
		  (* end case *)
		end
	    | doBlock _ = ()
	  fun keepBlock (blkId, _) = if null(in_edges blkId)
		then (remove_node blkId; false)
		else true
	  val blocks = if !disable
		then blocks
		else (
		  forall_nodes doBlock;
		  if !needFilter then List.filter keepBlock blocks else blocks)
	  in
	    if !dumpCFG
	      then CFG.dump(!dumpStrm, "after jump-chain elimination", cfg)
	      else ();
	    blocks
	  end

  end
