(* cfg.sml
 *
 * COPYRIGHT (c) 2002 Bell Labs, Lucent Technologies
 *
 * The control flow graph representation used for optimizations.
 *
 * -- Allen
 *)

functor ControlFlowGraph
   (structure I : INSTRUCTIONS
    structure GraphImpl : GRAPH_IMPLEMENTATION
    structure InsnProps : INSN_PROPERTIES where I = I
    structure Asm : INSTRUCTION_EMITTER where I = I
   ) : CONTROL_FLOW_GRAPH =
struct

    structure I = I
    structure P = Asm.S.P
    structure C = I.C
    structure G = Graph
    structure A = Annotations
    structure S = Asm.S

    type weight = real

    datatype block_kind = 
        START          (* entry node *)
      | STOP           (* exit node *)
      | NORMAL         (* normal node *)

    and block =
       BLOCK of
       {  id          : int,                        (* block id *)
          kind        : block_kind,                 (* block kind *)
          freq        : weight ref,                 (* execution frequency *) 
          labels      : Label.label list ref,       (* labels on blocks *) 
          insns       : I.instruction list ref,     (* in rev order *)
	  align	      : P.pseudo_op option ref,	    (* alignment only *)
          annotations : Annotations.annotations ref (* annotations *)
       }

    and edge_kind	    (* edge kinds (see cfg.sig for more info) *)
      = ENTRY			(* entry edge *) 
      | EXIT            	(* exit edge *)
      | JUMP			(* unconditional jump *)
      | FALLSTHRU		(* falls through to next block *)  
      | BRANCH of bool		(* branch *) 
      | SWITCH of int		(* computed goto *)
      | FLOWSTO			(* FLOW_TO edge *)
   
    and edge_info = EDGE of {
	k : edge_kind,                  (* edge kind *)
	w : weight ref,                 (* edge freq *)
	a : Annotations.annotations ref (* annotations *)
      }

    type edge = edge_info Graph.edge
    type node = block Graph.node

    datatype info = 
        INFO of { annotations : Annotations.annotations ref,
                  firstBlock  : int ref,
                  reorder     : bool ref,
		  data        : P.pseudo_op list ref
                }

    type cfg = (block,edge_info,info) Graph.graph

    fun error msg = MLRiscErrorMsg.error("ControlFlowGraph",msg)

   (*========================================================================
    *
    *  Various kinds of annotations 
    *
    *========================================================================*)
              (* escaping live out information *)
    val LIVEOUT = Annotations.new 
          (SOME(fn c => "Liveout: "^
                        (LineBreak.lineBreak 75 
                            (CellsBasis.CellSet.toString c))))
    exception Changed of string * (unit -> unit) 
    val CHANGED = Annotations.new'
          {create=Changed,
           get=fn Changed x => x | e => raise e,
           toString=fn (name,_) => "CHANGED:"^name
          }

   (*========================================================================
    *
    *  Methods for manipulating basic blocks
    *
    *========================================================================*)
    fun defineLabel(BLOCK{labels=ref(l::_),...}) = l
      | defineLabel(BLOCK{labels, ...}) = let
	  val l = Label.anon ()
          in
	    labels := [l];
	    l
	  end
    fun insns(BLOCK{insns, ...}) = insns
    fun freq(BLOCK{freq, ...}) = freq

    fun newBlock'(id,kind,insns,freq) =
        BLOCK{ id          = id,
               kind        = kind,
               freq        = freq,
               labels      = ref [],
               insns       = ref insns,
	       align       = ref NONE,
               annotations = ref []
             }

    fun copyBlock(id,BLOCK{kind,freq,align,labels,insns,annotations,...}) =
        BLOCK{ id          = id,
               kind        = kind,
               freq        = ref (!freq),
               labels      = ref [],
	       align	   = ref (!align),
               insns       = ref (!insns),
               annotations = ref (!annotations) 
             }

    fun newBlock(id,freq) = newBlock'(id,NORMAL,[],freq)
    fun newStart(id,freq) = newBlock'(id,START,[],freq)
    fun newStop(id,freq) = newBlock'(id,STOP,[],freq)

    fun branchOf(EDGE{k=BRANCH b,...}) = SOME b
      | branchOf _ = NONE
    fun edgeDir(_,_,e) = branchOf e

   (*========================================================================
    *
    *  Emit a basic block
    *
    *========================================================================*)
    fun kindName START          = "START"
      | kindName STOP           = "STOP"
      | kindName NORMAL         = "Block"

    fun nl() = TextIO.output(!AsmStream.asmOutStream,"\n")

    fun emitHeader (S.STREAM{comment,annotation,...}) 
                   (BLOCK{id,kind,freq,annotations,...}) = 
       (comment(kindName kind ^"["^Int.toString id^
                    "] ("^Real.toString (!freq)^")");
        nl();
        app annotation (!annotations)
       ) 

    fun emitFooter (S.STREAM{comment,...}) (BLOCK{annotations,...}) = 
        (case #get LIVEOUT (!annotations) of
            SOME s => 
            let val regs = String.tokens Char.isSpace(CellsBasis.CellSet.toString s)
                val K = 7
                fun f(_,[],s,l)    = s::l
                  | f(0,vs,s,l)    = f(K,vs,"   ",s::l)
                  | f(n,[v],s,l)   = v^s::l
                  | f(n,v::vs,s,l) = f(n-1,vs,s^" "^v,l)
                val text = rev(f(K,regs,"",[]))
            in  app (fn c => (comment c; nl())) text
            end
         |  NONE => ()
        ) handle Overflow => print("Bad footer\n")

    fun emitStuff outline annotations 
           (block as BLOCK{insns,labels,...}) =
       let val S as S.STREAM{pseudoOp,defineLabel,emit,...} = 
               Asm.makeStream annotations
       in  emitHeader S block;
           app defineLabel (!labels); 
           if outline then () else app emit (rev (!insns));
           emitFooter S block
       end

    val emit = emitStuff false 
    val emitOutline = emitStuff true []
 
   (*========================================================================
    *
    *  Methods for manipulating CFG
    *
    *========================================================================*)
    fun cfg info = GraphImpl.graph("CFG",info,10)
    fun new() =
        let val info = INFO{ annotations = ref [],
                             firstBlock  = ref 0,
                             reorder     = ref false,
			     data        = ref []
                           }
        in  cfg info end

    fun subgraph(CFG as G.GRAPH{graph_info=INFO graph_info,...}) =
        let val info = INFO{ annotations = ref [],
                             firstBlock  = #firstBlock graph_info,
                             reorder     = #reorder graph_info,
			     data        = #data graph_info
                           }
        in  UpdateGraphInfo.update CFG info end

    fun init(G.GRAPH cfg) =
        (case #entries cfg () of
           [] =>
           let val i     = #new_id cfg ()
               val start = newStart(i,ref 0.0)
               val _     = #add_node cfg (i,start)
               val j     = #new_id cfg ()
               val stop  = newStop(j,ref 0.0)
               val _     = #add_node cfg (j,stop) 
           in (*  #add_edge cfg (i,j,EDGE{k=ENTRY,w=ref 0,a=ref []}); *)
               #set_entries cfg [i];
               #set_exits cfg [j]
           end
        |  _ => () 
        )

    fun changed(G.GRAPH{graph_info=INFO{reorder,annotations,...},...}) = 
        let fun signal [] = ()
              | signal(Changed(_,f)::an) = (f (); signal an)
              | signal(_::an) = signal an
        in  signal(!annotations);
            reorder := true
        end 

    fun annotations(G.GRAPH{graph_info=INFO{annotations=a,...},...}) = a

    fun liveOut (BLOCK{annotations, ...}) = 
         case #get LIVEOUT (!annotations) of
            SOME s => s
         |  NONE => C.empty
    fun fallsThruFrom(G.GRAPH cfg,b) =
        let fun f [] = NONE
              | f((i,_,EDGE{k=BRANCH false,...})::_) = SOME i
              | f((i,_,EDGE{k=FALLSTHRU,...})::_) = SOME i
              | f(_::es) = f es
        in  f(#in_edges cfg b)
        end
    fun fallsThruTo(G.GRAPH cfg,b) =
        let fun f [] = NONE
              | f((_,j,EDGE{k=BRANCH false,...})::_) = SOME j
              | f((_,j,EDGE{k=FALLSTHRU,...})::_) = SOME j
              | f(_::es) = f es
        in  f(#out_edges cfg b)
        end
    fun removeEdge CFG (i,j,EDGE{a,...}) =
        Graph.remove_edge' CFG (i,j,fn EDGE{a=a',...} => a = a')

    fun setBranch (CFG as G.GRAPH cfg,b,cond) =
    let fun loop((i,j,EDGE{k=BRANCH cond',w,a})::es,es',x,y) =
            if cond' = cond then 
               loop(es, (i,j,EDGE{k=JUMP,w=w,a=a})::es',j,y)
            else
               loop(es, es', x, j)
          | loop([],es',target,elim) = (es',target,elim)
          | loop _ = error "setBranch"
        val outEdges = #out_edges cfg b
        val (outEdges',target,elim) = loop(outEdges,[],~1,~1)
        val _ = if elim < 0 then error "setBranch: bad edges" else ();
        val lab = defineLabel(#node_info cfg target) 
        val jmp = InsnProps.jump lab
        val insns = insns(#node_info cfg b) 
    in  #set_out_edges cfg (b,outEdges');
        case !insns of
          []      => error "setBranch: missing branch"
        | branch::rest => 
           case InsnProps.instrKind branch of
             InsnProps.IK_JUMP => insns := jmp::rest
           | _ => error "setBranch: bad branch instruction";
        jmp
    end

   (*========================================================================
    *
    *  Miscellaneous 
    *
    *========================================================================*)
   fun cdgEdge(EDGE{k, ...}) = 
        case k of
           (JUMP | FALLSTHRU) => false
        |  _ => true

   (*========================================================================
    *
    *  Pretty Printing and Viewing 
    *
    *========================================================================*)

    structure F = Format

    fun show_edge (EDGE{k,w,a,...}) = let
	  val kind = (case k
		 of JUMP	=> "jump"
        	  | FALLSTHRU	=> "fallsthru"
        	  | BRANCH b	=> Bool.toString b
        	  | SWITCH i	=> Int.toString i
        	  | ENTRY	=> "entry"
        	  | EXIT	=> "exit"
        	  | FLOWSTO	=> "flowsto"
		(* end case *))
	  in
	    F.format "%s(%f)" [F.STR kind, F.REAL(!w)]
	  end

    fun getString f x = let
	  val buffer = StringOutStream.mkStreamBuf()
	  val S      = StringOutStream.openStringOut buffer
	  val _      = AsmStream.withStream S f x 
	  in
	    StringOutStream.getString buffer
	  end

    fun show_block an block = let
	  val text = getString (emit an) block
	  in
	    foldr (fn (x,"") => x | (x,y) => x^" "^y) ""
              (String.tokens (fn #" " => true | _ => false) text)
	  end

    fun dumpBlock (outS, cfg as G.GRAPH g) = let
	  fun pr str = TextIO.output(outS, str)
	  fun prList [] = ()
	    | prList [i] = pr i
	    | prList (h::t) = (pr (h ^ ", "); prList t)
	  val Asm.S.STREAM{emit,defineLabel,annotation,...} = 
        	AsmStream.withStream outS Asm.makeStream []
	  fun showFreq (ref w) = F.format "[%f]" [F.REAL w]
	  fun showEdge (blknum,e) = 
		F.format "%d:%s" [F.INT blknum, F.STR(show_edge e)]
	  fun showSucc (_, x, e) = showEdge(x,e)
	  fun showPred (x, _, e) = showEdge(x,e) 
	  fun showSuccs b = (
		pr "\tsucc:     "; 
        	prList (map showSucc (#out_edges g b)); 
        	pr "\n")
	  fun showPreds b = (
        	pr "\tpred:     "; 
        	prList (map showPred (#in_edges g b)); 
        	pr "\n")
	  fun printBlock (_, BLOCK{kind=START, id, freq, ...}) = (
        	pr (F.format "ENTRY %d %s\n" [F.INT id, F.STR(showFreq freq)]);
        	showSuccs id)
            | printBlock (_, BLOCK{kind=STOP, id, freq, ...}) = (
		pr (F.format "EXIT %d %s\n" [F.INT id, F.STR(showFreq freq)]);
        	showPreds id)
            | printBlock (
		_, BLOCK{id, align, freq, insns, annotations, labels, ...}
	      ) = (
	       pr (F.format "BLOCK %d %s\n" [F.INT id, F.STR(showFreq freq)]);
	       case !align of NONE => () | SOME p => (pr (P.toString p ^ "\n"));
               List.app annotation (!annotations);
               List.app defineLabel (!labels);
               showSuccs id;
               showPreds id;
               List.app emit (List.rev (!insns)))
	  in
	    printBlock
	  end

    fun dump (outS, title, cfg as G.GRAPH g) = let
	  fun pr str = TextIO.output(outS, str)
	  val annotations = !(annotations cfg)
	  val Asm.S.STREAM{annotation, ...} = 
        	AsmStream.withStream outS Asm.makeStream annotations
	  fun printData () = let
        	val INFO{data, ...} = #graph_info g
		in
		  List.app (pr o P.toString) (rev(!data))
		end
	  in
	    pr(F.format "[ %s ]\n" [F.STR title]);
	    List.app annotation annotations;
	    (* printBlock entry; *)
	    AsmStream.withStream outS (#forall_nodes g) (dumpBlock (outS, cfg));
	    (* printBlock exit; *)
	    AsmStream.withStream outS printData ();
	    TextIO.flushOut outS
	  end

end

