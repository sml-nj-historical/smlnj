(* flowgraph.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Defines the flowgraph data structure used as the intermediate program
 * representation.
 *
 *)

signature FLOWGRAPH = sig

  structure C : CELLS
  structure I : INSTRUCTIONS
  structure P : PSEUDO_OPS
	  sharing I.C = C


  datatype block =
      PSEUDO of P.pseudo_op
    | LABEL of Label.label
    | BBLOCK of { blknum  : int,
		  liveIn  : C.cellset ref,
		  liveOut : C.cellset ref,
		  succ 	  : block list ref,
		  pred 	  : block list ref,
		  insns	  : I.instruction list ref
	        }
    | ENTRY of {blknum : int,
		succ : block list ref}
    | EXIT of {blknum : int,
	       pred : block list ref}
    | ORDERED of block list

  datatype cluster = 
      CLUSTER of {
        blocks: block list, 
	entry : block,
	exit  : block,	  
        regmap: int Intmap.intmap,
        blkCounter : int ref
      }

  val prBlock : block -> unit
end


(*  Create the flowgraph data structure specialized towards	
 *  a specific type of cells and instructions.
 *)
functor FlowGraph(structure I : INSTRUCTIONS
		  structure P : PSEUDO_OPS) : FLOWGRAPH = 
struct
  structure I = I
  structure C = I.C
  structure P = P

  datatype block =
      PSEUDO of P.pseudo_op
    | LABEL of Label.label
    | BBLOCK of { blknum  : int,
		  liveIn  : C.cellset ref,
		  liveOut : C.cellset ref,
		  succ 	  : block list ref,
		  pred 	  : block list ref,
		  insns	  : I.instruction list ref
	        }
    | ENTRY of {blknum : int,
		succ : block list ref}
    | EXIT of {blknum : int,
	       pred : block list ref}
    | ORDERED of block list

  datatype cluster = 
      CLUSTER of {
        blocks: block list, 
	entry : block,
	exit  : block,	  
        regmap: int Intmap.intmap,
        blkCounter : int ref
      }

  fun prBlock(PSEUDO pOp)        = print (P.toString pOp)
    | prBlock(LABEL lab)         = print ("LABEL " ^ Label.nameOf lab ^ "\n")
    | prBlock(BBLOCK{blknum,succ,pred,liveOut,liveIn,...}) = let

	fun prBlkList [] = print "\n"
	  | prBlkList (BBLOCK{blknum, ...}::blocks) =
	      (print (Int.toString blknum ^ ","); prBlkList blocks)

	fun prCells cells = 
	  (print (C.cellset2string cells); 
	   print "\n")
      in
	  print("BLOCK" ^ Int.toString blknum ^ "\n");
	  print("\t liveIn: ");  prCells(!liveIn);
	  print("\t succ: ");    prBlkList(!succ);
	  print("\t pred: ");    prBlkList(!pred);
	  print("\t liveOut: "); prCells(!liveOut)
      end
    | prBlock(ORDERED blks) = app prBlock blks
end




(*
 * $Log: flowgraph.sml,v $
 * Revision 1.4  1997/09/17 17:13:49  george
 *   Removed the CODE constructor used for code compression and added
 *   and ENTRY and EXIT basic block. The cluster datatype has also been
 *   enriched.
 *
# Revision 1.3  1997/07/17  12:29:25  george
#   The regmap is now represented as an int map rather than using arrays.
#
# Revision 1.2  1997/07/10  04:01:18  george
#   Added ORDERED as a new flowgraph constructor.
#
# Revision 1.1.1.1  1997/04/19  18:14:20  george
#   Version 109.27
#
 *)
