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
  structure W : FREQ
	  sharing I.C = C

  datatype block =
      PSEUDO of P.pseudo_op
    | LABEL of Label.label
    | BBLOCK of { blknum      : int,
                  freq        : W.freq ref,
                  annotations : Annotations.annotations ref,
		  liveIn      : C.cellset ref,
		  liveOut     : C.cellset ref,
		  succ 	      : edge list ref,
		  pred 	      : edge list ref,
		  insns	      : I.instruction list ref
	        }
    | ENTRY of {blknum : int, freq : W.freq ref, succ : edge list ref}
    | EXIT of {blknum : int, freq : W.freq ref, pred : edge list ref}
  withtype edge = block * W.freq ref

  datatype cluster = 
      CLUSTER of {
        blocks: block list, 
	entry : block,
	exit  : block,	  
        blkCounter : int ref,
        annotations : Annotations.annotations ref
      }
end


(*  Create the flowgraph data structure specialized towards	
 *  a specific type of cells and instructions.
 *)
functor FlowGraph(structure I : INSTRUCTIONS
		  structure P : PSEUDO_OPS
		 ) : FLOWGRAPH = 
struct
  structure I = I
  structure C = I.C
  structure P = P
  structure W = Freq

  datatype block =
      PSEUDO of P.pseudo_op
    | LABEL of Label.label
    | BBLOCK of { blknum      : int,
                  freq        : W.freq ref,
                  annotations : Annotations.annotations ref,
		  liveIn      : C.cellset ref,
		  liveOut     : C.cellset ref,
		  succ 	      : edge list ref,
		  pred 	      : edge list ref,
		  insns	      : I.instruction list ref
	        }
    | ENTRY of {blknum : int, freq : W.freq ref, succ : edge list ref}
    | EXIT of {blknum : int, freq : W.freq ref, pred : edge list ref}
  withtype edge = block * W.freq ref

  datatype cluster = 
      CLUSTER of {
        blocks: block list, 
	entry : block,
	exit  : block,	  
        blkCounter : int ref,
        annotations : Annotations.annotations ref
      }
end
