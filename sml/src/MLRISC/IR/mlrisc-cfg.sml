(*
 * The control flow graph representation used for optimizations.
 *
 * -- Allen
 *)
functor ControlFlowGraph
   (structure I : INSTRUCTIONS
    structure PseudoOps : PSEUDO_OPS
    structure GraphImpl : GRAPH_IMPLEMENTATION
    structure Asm : INSTRUCTION_EMITTER
       sharing Asm.I = I
       sharing Asm.P = PseudoOps
   ) : CONTROL_FLOW_GRAPH =
struct

    structure I = I
    structure P = PseudoOps
    structure C = I.C
    structure W = Freq
    structure G = Graph
    structure L = GraphLayout
    structure A = Annotations
    structure S = Asm.S
   
    type weight = W.freq

    datatype block_kind = 
        START          (* entry node *)
      | STOP           (* exit node *)
      | FUNCTION_ENTRY (* for SSA transformations *)
      | NORMAL         (* normal node *)
      | HYPERBLOCK     (* hyperblock *)

    and data = LABEL  of Label.label
             | PSEUDO of P.pseudo_op
 
    and block = 
       BLOCK of
       {  id          : int,                        (* block id *)
          kind        : block_kind,                 (* block kind *)
          freq        : weight ref,                 (* execution frequency *) 
          data        : data list ref,              (* data preceeding block *) 
          labels      : Label.label list ref,       (* labels on blocks *) 
          insns       : I.instruction list ref,     (* in rev order *)
          annotations : Annotations.annotations ref (* annotations *)
       }

    and edge_kind = ENTRY           (* entry edge *) 
                  | EXIT            (* exit edge *)
                  | JUMP            (* unconditional jump *)
                  | FALLSTHRU       (* falls through to next block *)  
                  | BRANCH of bool  (* branch *) 
                  | SWITCH of int   (* computed goto *)   
                  | SIDEEXIT of int (* side exit *)   
   
    and edge_info = EDGE of { k : edge_kind,                  (* edge kind *)
                              w : weight ref,                 (* edge freq *)
                              a : Annotations.annotations ref (* annotations *)
                            }

    type edge = edge_info Graph.edge
    type node = block Graph.node

    datatype info = 
        INFO of { regmap      : C.regmap,
                  annotations : Annotations.annotations ref,
                  firstBlock  : int ref,
                  reorder     : bool ref
                }

    type cfg = (block,edge_info,info) Graph.graph

   (*========================================================================
    *
    *  Various kinds of annotations 
    *
    *========================================================================*)
              (* escaping live out information *)
    val LIVEOUT = Annotations.new 
          (SOME(fn c => "Liveout: "^
                        (LineBreak.lineBreak 75 (C.cellsetToString c))))
    val CHANGED = Annotations.new(SOME(fn (f : unit -> unit) => "CHANGED"))
    val CHANGEDONCE = Annotations.new
                        (SOME(fn (f : unit -> unit) => "CHANGEDONCE"))

   (*========================================================================
    *
    *  Methods for manipulating basic blocks
    *
    *========================================================================*)
    fun defineLabel(BLOCK{labels=ref(l::_),...}) = l
      | defineLabel(BLOCK{labels,...}) = let val l = Label.newLabel ""
                                         in  labels := [l]; l end

    fun newBlock'(id,kind,insns,freq) =
        BLOCK{ id          = id,
               kind        = kind,
               freq        = freq,
               data        = ref [],
               labels      = ref [],
               insns       = ref insns,
               annotations = ref []
             }

    fun copyBlock(id,BLOCK{kind,freq,data,labels,insns,annotations,...}) =
        BLOCK{ id          = id,
               kind        = kind,
               freq        = ref (!freq),
               data        = ref (!data),
               labels      = ref [],
               insns       = ref (!insns),
               annotations = ref (!annotations) 
             }

    fun newBlock(id,freq) = newBlock'(id,NORMAL,[],freq)
    fun newStart(id,freq) = newBlock'(id,START,[],freq)
    fun newStop(id,freq) = newBlock'(id,STOP,[],freq)
    fun newFunctionEntry(id,freq) = newBlock'(id,FUNCTION_ENTRY,[],freq)

   (*========================================================================
    *
    *  Emit a basic block
    *
    *========================================================================*)
    fun kindName START          = "START"
      | kindName STOP           = "STOP"
      | kindName HYPERBLOCK     = "Hyperblock"
      | kindName FUNCTION_ENTRY = "Entry"
      | kindName NORMAL         = "Block"

    fun nl() = TextIO.output(!AsmStream.asmOutStream,"\n")

    fun emitHeader (S.STREAM{comment,annotation,...}) 
                   (BLOCK{id,kind,freq,annotations,...}) = 
       (comment(kindName kind ^"["^Int.toString id^
                    "] ("^W.toString (!freq)^")");
        nl();
        app annotation (!annotations)
       ) 

    fun emitFooter (S.STREAM{comment,...}) (BLOCK{annotations,...}) = 
        (case #get LIVEOUT (!annotations) of
            SOME s => 
            let val regs = String.tokens Char.isSpace(C.cellsetToString s)
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

    fun emitStuff outline annotations regmap
           (block as BLOCK{insns,data,labels,...}) =
       let val S as S.STREAM{pseudoOp,defineLabel,emit,...} = 
               Asm.makeStream annotations
           val emit = emit (I.C.lookup regmap)
       in  emitHeader S block;
           app (fn PSEUDO p => pseudoOp p
                 | LABEL l  => defineLabel l) (!data);
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
    fun new(regmap) =
        let val info = INFO{ regmap      = regmap,  
                             annotations = ref [],
                             firstBlock  = ref 0,
                             reorder     = ref false
                           }
        in  cfg info end

    fun subgraph(CFG as G.GRAPH{graph_info=INFO graph_info,...}) =
        let val info = INFO{ regmap      = #regmap graph_info,
                             annotations = ref [],
                             firstBlock  = #firstBlock graph_info,
                             reorder     = #reorder graph_info
                           }
        in  UpdateGraphInfo.update CFG info end

    fun init(G.GRAPH cfg) =
        (case #entries cfg () of
           [] =>
           let val i     = #new_id cfg ()
               val start = newStart(i,ref 0)
               val _     = #add_node cfg (i,start)
               val j     = #new_id cfg ()
               val stop  = newStop(i,ref 0)
               val _     = #add_node cfg (j,stop) 
           in  #add_edge cfg (i,j,EDGE{k=ENTRY,w=ref 0,a=ref []});
               #set_entries cfg [i];
               #set_exits cfg [j]
           end
        |  _ => () 
        )

    fun changed(G.GRAPH{graph_info=INFO{reorder,annotations,...},...}) = 
         (case #get CHANGED (!annotations) of
            SOME f => f ()
          | NONE => ();
          reorder := true
         )

    fun regmap(G.GRAPH{graph_info=INFO{regmap,...},...}) = regmap

    fun setAnnotations(G.GRAPH{graph_info=INFO{annotations,...},...},a) = 
        annotations := a

    fun getAnnotations(G.GRAPH{graph_info=INFO{annotations=ref a,...},...}) = a

    fun reglookup cfg = C.lookup(regmap cfg)

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
   fun show_edge(EDGE{k,w,a,...}) = 
       let val kind = case k of
                         JUMP      => ""
                      |  FALLSTHRU => "fallsthru"
                      |  BRANCH b => Bool.toString b
                      |  SWITCH i => Int.toString i
                      |  ENTRY    => "entry"
                      |  EXIT     => "exit"
                      |  SIDEEXIT i => "sideexit("^Int.toString i^")"
           val weight = "(" ^ W.toString (!w) ^ ")"
       in  kind ^ weight 
       end 

   fun getString f x = 
   let val buffer = StringOutStream.mkStreamBuf()
       val S      = StringOutStream.openStringOut buffer
       val _      = AsmStream.withStream S f x 
   in  StringOutStream.getString buffer end

   fun show_block an regmap block = 
   let val text = getString (emit an regmap) block
   in  foldr (fn (x,"") => x | (x,y) => x^" "^y) ""
            (String.tokens (fn #" " => true | _ => false) text)
   end

   fun headerText block = getString 
        (fn b => emitHeader (Asm.makeStream []) b) block
   fun footerText block = getString 
        (fn b => emitFooter (Asm.makeStream []) b) block

   fun getStyle a = (case #get L.STYLE (!a) of SOME l => l | NONE => [])

   fun edgeStyle(i,j,e as EDGE{k,a,...}) = 
   let val a = L.LABEL(show_edge e) :: getStyle a
   in  case k of 
         (ENTRY | EXIT) => L.COLOR "green" :: a
       | _ => L.COLOR "red" :: a
   end 

   val outline = MLRiscControl.getFlag "view-outline"

   fun viewStyle cfg =
   let val regmap = regmap cfg
       val an     = getAnnotations cfg
       fun node (n,b as BLOCK{annotations,...}) = 
           if !outline then
              L.LABEL(getString (emitOutline regmap) b) :: getStyle annotations
           else
              L.LABEL(show_block an regmap b) :: getStyle annotations
   in  { graph = fn _ => [],
         edge  = edgeStyle,
         node  = node
       } 
   end

   fun viewLayout cfg = L.makeLayout (viewStyle cfg) cfg

   fun subgraphLayout {cfg,subgraph = G.GRAPH subgraph} =
   let val regmap = regmap cfg
       val an     = getAnnotations cfg
       fun node(n,b as BLOCK{annotations,...}) = 
          if #has_node subgraph n then
             L.LABEL(show_block an regmap b) :: getStyle annotations
          else
             L.COLOR "lightblue"::L.LABEL(headerText b) :: getStyle annotations
       fun edge(i,j,e) = 
            if #has_edge subgraph (i,j) then edgeStyle(i,j,e)
            else [L.EDGEPATTERN "dotted"]
   in  L.makeLayout {graph = fn _ => [],
                     edge  = edge,
                     node  = node} cfg
   end

end

