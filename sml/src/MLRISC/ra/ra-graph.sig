(*
 * This is the new interference graph used by the new register allocator.
 * 
 * -- Allen
 *)

signature RA_GRAPH = 
sig

  (*
   * The following are the data structures used in the register allocator.
   *)

  (* A new bit matrix datatype.
   * We use the small representation whenever possible to save space.
   *)
  datatype bitMatrix = BM of {table:hashTable, 
                              elems:int ref,
                              edges:int}
  and hashTable = SMALL of word list Array.array ref * word
                | LARGE of bucket Array.array ref * word
             (* | BITMATRIX of Word8Array.array *)
  and bucket = NIL | B of int * int * bucket 

  exception Nodes

  type priority = int     
  type cost = int         

  (*
   * The following represent a program point in the program.
   * As a convention, program point is computed by
   *
   *    block number * 16384 + instruction number
   *
   * The last instruction in the block is numbered 1, i.e. the instruction
   * numbering is in reverse.  The number 0 is reserved for "live-out".
   *
   * This implies that there can be a maximum of 16k-1 instructions
   * per basic block/hyperblock, plus a maximum of 32k blocks.
   * Let's hope this is enough. (I'm not kidding, aggressive inlining
   * and unrolling can produce large blocks.)
   *)
  type programPoint = int 

  datatype interferenceGraph = 
     GRAPH of 
     { bitMatrix    : bitMatrix ref,
       nodes        : node Intmap.intmap,
       regmap       : int Intmap.intmap,
       K            : int,
       firstPseudoR : int,
       dedicated    : bool Array.array,
       getreg       : {pref:int list, stamp:int, proh:int Array.array} -> int,
       getpair      : {pref:int list, stamp:int, proh:int Array.array} -> int,
       proh         : int Array.array,
       stamp        : int ref,

       (* Info to undo a spill when an optimistic spill has occurred *)
       spillFlag    : bool ref,

       spilledRegs  : bool Intmap.intmap, (*registers that have been spilled*)
       trail        : trailInfo ref,

       (* how to pretty print a register *)
       showReg      : int -> string,

       (* how many registers there are? *)
       numRegs      : int,
       maxRegs      : unit -> int,

       (* dead copies *)
       deadCopies   : int list ref,

       (* spill locations *)
       spillLoc     : int ref
     }

  and moveStatus = MOVE         (* not yet coalesced *)
                 | COALESCED    (* coalesced *)
                 | CONSTRAINED  (* src and target intefere *)
                 | LOST         (* frozen moves *)
                 | WORKLIST     (* on the move worklist *)

  and move = 
    MV of {src    : node,  		(* source register of move *)
	   dst    : node,		(* destination register of move *)
           (*kind   : moveKind, *)      (* kind of move *)
           cost   : cost,               (* cost *)
	   status : moveStatus ref	(* coalesced? *)
	  }

  and moveKind = REG_TO_REG      (* register to register *)
               | EVEN_TO_REG     (* even register in pair to register *)
               | ODD_TO_REG      (* odd register in pair to register *)
               | PAIR_TO_PAIR    (* register pair to register pair *)
               | REG_TO_EVEN     (* register to even register in pair *)
               | REG_TO_ODD      (* register to odd register in pair *)

  and nodeStatus = 
        PSEUDO                (* pseudo register *)
      | REMOVED               (* removed from the interference graph *)
      | ALIASED of node       (* coalesced *)
      | COLORED of int        (* colored *)
      | SPILLED of int        (* spilled *)
      | ALIASED_SPILL of node (* aliased spill node *)

  and node = 
    NODE of { number : int,		(* node number *)
	      movecnt: int ref,		(* #moves this node is involved in *)
	      movelist: move list ref,	(* moves associated with this node *)
	      degree : int ref,		(* current degree *)
	      color : nodeStatus ref,	(* status *)
	      adj : node list ref,	(* adjacency list *)
              pri : priority ref,       (* priority *)
              movecost : cost ref,      (* move cost *)
              (* pair : bool, *)        (* register pair? *)
              defs : programPoint list ref, 
              uses : programPoint list ref
            }

  and trailInfo = END | UNDO of node * moveStatus ref * trailInfo

  (* Create a new bitMatrix *)
  val newBitMatrix : {edges : int, maxRegs : int} -> bitMatrix

  (* Create a new interference graph *)
  val newGraph : { nodes        : node Intmap.intmap,
                   regmap       : int Intmap.intmap,
                   numRegs      : int,
                   maxRegs      : unit -> int,
                   K            : int,
                   firstPseudoR : int,
                   dedicated    : bool Array.array,
                   showReg      : int -> string,
                   getreg       : 
                     {pref:int list,stamp:int,proh:int Array.array} -> int,
                   getpair      : 
                     {pref:int list,stamp:int,proh:int Array.array} -> int,
                   proh         : int Array.array
                 } -> interferenceGraph

end
