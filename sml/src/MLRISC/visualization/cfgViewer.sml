functor CFGViewer
   (structure CFG : CONTROL_FLOW_GRAPH
    structure GraphViewer : GRAPH_VIEWER
    structure Asm	  : INSTRUCTION_EMITTER where I = CFG.I)
      : sig
	    val view : CFG.cfg -> unit
	end =
struct
   structure CFG = CFG
   structure L = GraphLayout
   structure FMT = FormatInstruction(Asm)
   structure G = Graph

   val outline = MLRiscControl.getFlag "view-outline"

   fun view(cfg as G.GRAPH g) = let
       val CFG.INFO{annotations, ...} = #graph_info g
       val toString = FMT.toString (!annotations)
       fun graph _ = []

       val red = L.COLOR "red"
       val yellow = L.COLOR "yellow"
       val green = L.COLOR "green"

       val ENTRY = hd(#entries g ())
       val EXIT  = hd(#exits g ())

       fun edge(i,j,CFG.EDGE{w, ...}) = 
       let val label = L.LABEL(Real.toString (!w))
           val color =
               if i = ENTRY orelse j = EXIT then green (* special edge *)
               else if i+1 = j then yellow (* fallsthru *)
               else red
       in  [label, color] end

       fun title(blknum,ref freq) = 
           " "^Int.toString blknum^" ("^Real.toString freq^")"

       fun ann(annotations) = 
            List.foldl(fn (a,l) => "/* "^Annotations.toString a^" */\n"^l) ""
                             (!annotations)

       fun node(_, CFG.BLOCK{kind, id, freq, insns, annotations, ...}) = 
	 (case kind
	   of CFG.START => 
	        [L.LABEL("entry"^title(id,freq)^"\n"^ann(annotations))]
	    | CFG.STOP  => 
                [L.LABEL("exit"^title(id,freq))]
	    | _ => 
              [L.LABEL(title(id,freq)^"\n"^
                 ann(annotations)^
                 (if !outline then "" else
                 List.foldl (fn (i,t) => 
                             let val text = toString i
                             in  if text = "" then t else text^"\n"^t end
                            ) "" (!insns)))]
	  (*esac*))

   in  
      GraphViewer.view
           (L.makeLayout{graph=graph, edge=edge, node=node} cfg)
   end

end
