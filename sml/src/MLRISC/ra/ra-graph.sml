(*
 * This is the new interference graph used by the new register allocator.
 * 
 * -- Allen
 *)

structure RAGraph : RA_GRAPH =
struct

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

  type priority = int

  type programPoint = int

  type cost = int

  type mode = word

  datatype interferenceGraph = 
   GRAPH of { bitMatrix    : bitMatrix ref,
              nodes        : node Intmap.intmap,
              regmap       : int Intmap.intmap,
              K            : int,
              firstPseudoR : int,
              dedicated    : bool Array.array,
              getreg       : 
                  {pref:int list, stamp:int, proh:int Array.array} -> int,
              getpair       : 
                  {pref:int list, stamp:int, proh:int Array.array} -> int,
              proh         : int Array.array,
              stamp        : int ref,

             (* Info to undo a spill when an optimistic spill has occurred *)
              spillFlag    : bool ref,
              spilledRegs  : bool Intmap.intmap,
              trail        : trailInfo ref,

              showReg      : int -> string,
              numRegs      : int,
              maxRegs      : unit -> int,
 
              deadCopies   : int list ref,
              copyTmps     : node list ref,
              memMoves     : move list ref,
              memRegs      : node list ref,

              spillLoc     : int ref,
              span         : int Intmap.intmap option ref,
              mode         : mode,
              pseudoCount  : int ref,
              blockedCount : int ref
            }

  and moveStatus = BRIGGS_MOVE | GEORGE_MOVE
                 | COALESCED | CONSTRAINED | LOST | WORKLIST

  and move = 
    MV of {src : node,			(* source register of move *)
	   dst : node,			(* destination register of move *)
           (* kind: moveKind, *)        (* what kind of move *)
           cost : cost,                 (* cost *)
	   status : moveStatus ref,	(* coalesced? *)
	   hicount : int ref	        (* neighbors of high degree *)
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

  and node = 
    NODE of { number : int,		(* node number *)
	      movecnt: int ref,		(* #moves this node is involved in *)
	      movelist: move list ref,	(* moves associated with this node *)
	      degree : int ref,		(* current degree *)
	      color : nodeStatus ref,	(* status *)
	      adj : node list ref,      (* adjacency list *)
              pri : priority ref,       (* priority *)
              movecost : cost ref,      (* move cost *)
              (* pair : bool, *)        (* register pair? *)
              defs : programPoint list ref,
              uses : programPoint list ref
            }

  and trailInfo = END | UNDO of node * moveStatus ref * trailInfo

  exception Nodes

  fun error msg = MLRiscErrorMsg.error("NewRAGraph", msg)

  val stampCounter = ref 0

  (* Create a new bitMatrix *)
  fun roundSize size = 
  let fun f(x, shift) =
        if x >= size then (x, Word.>>(shift, 0w1))
        else f(x+x, shift+0w1)
  in f(64, 0w6) end

  val max = Word.<<(0w1,Word.>>(Word.fromInt Word.wordSize,0w1)) 
  val _ = if max < Word.<<(0w1,0w15) 
          then error "word size too small" else ()

  fun newBitMatrix{edges, maxRegs} =
  let val table = 
        (* if maxRegs < 1024 then
          let val denseBytes  = (maxRegs * (maxRegs + 1) + 15) div 16
          in  BITMATRIX(Word8Array.array(denseBytes,0w0))
          end 
          else *)
          let val (tableSize, shift) = roundSize edges
          in  if Word.fromInt maxRegs < max then
                 SMALL(ref(Array.array(tableSize,[])),shift)
              else  
                 LARGE(ref(Array.array(tableSize,NIL)),shift)
          end
  in  BM{table=table, elems=ref 0, edges=edges}
  end

  (* Create a new interference graph *)
  fun newGraph{nodes,regmap,K,firstPseudoR,dedicated,spillLoc,
               getreg,getpair,showReg,maxRegs,numRegs,proh,
               memRegs,mode} =
  let (* lower triangular bitmatrix primitives *)
      (* NOTE: The average ratio of E/N is about 16 *)
      val bitMatrix = newBitMatrix{edges=numRegs * 16,maxRegs=maxRegs()}

      (* Make memory register nodes *)
      fun makeMemRegs [] = []
        | makeMemRegs(ranges) = 
          let val add = Intmap.add nodes
              fun loop(from, to, ns) = 
                  if from > to then ns 
                  else 
                  let val node = 
                      NODE{pri=ref 0,adj=ref [],degree=ref 0,movecnt=ref 0,
                           color=ref(SPILLED from), defs=ref [], uses=ref [],
                           movecost=ref 0,movelist=ref [], number=from}
                  in  add(from, node); loop(from+1, to, node::ns)
                  end
              fun loop2([], ns) = ns
                | loop2((from,to)::ranges, ns) = loop2(ranges, loop(from,to,ns))
          in  loop2(ranges, [])
          end 

      val memRegs = makeMemRegs memRegs

  in  if !stampCounter > 10000000 then stampCounter := 0 else ();
      GRAPH{ bitMatrix    = ref bitMatrix,
             nodes        = nodes,
             regmap       = regmap,
             K            = K,
             firstPseudoR = firstPseudoR,
             dedicated    = dedicated,
             getreg       = getreg,
             getpair      = getpair,
             proh         = proh,
             stamp        = stampCounter,
             spillFlag    = ref false,
             spilledRegs  = Intmap.new(2,Nodes),
             trail        = ref END,
             showReg      = Int.toString,
             numRegs      = numRegs,
             maxRegs      = maxRegs,
             deadCopies   = ref [],
             copyTmps     = ref [],
             memMoves     = ref [],
             memRegs      = ref memRegs,
             spillLoc     = spillLoc,
             span         = ref NONE,
             mode         = mode,
             pseudoCount  = ref 0,
             blockedCount = ref 0
           }
  end
  
end
