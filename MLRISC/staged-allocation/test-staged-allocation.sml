structure TestStagedAllocation =
  struct
 
    structure C = AMD64Instr.C
    structure T = AMD64MLTree
    structure CFG = AMD64CFG

    val wordTy = 64
   
    fun codegen (functionName, target, proto, initStms, args) = let 
        val _ = Label.reset()

	fun toLabel s = Label.global(s)
	val [functionName, target] = List.map toLabel [functionName, target]

        val insnStrm = AMD64FlowGraph.build()
	(* construct the C call *)
	val {result, callseq} = CCalls.genCall {
	           name=T.LABEL target,
	           paramAlloc=fn _ => false,
	           structRet=fn _ => T.REG (64, C.rax),
	           saveRestoreDedicated=fn _ => {save=[], restore=[]},
	           callComment=NONE,
	           proto=proto,
	           args=args}

	fun wordLit i = T.LI (T.I.fromInt (wordTy, i))

	fun mv () = let
	    val r = C.newReg()
	    in
	        [T.MV(wordTy, r, T.LOAD(wordTy, T.REG(wordTy, C.rsp), ())),
		 T.MV(wordTy, C.rax, T.REG(wordTy, r))
		]
	    end

	val fr = C.FPReg 10

	fun fmv () = let
	    val r = C.newFreg()
	    in
	        [T.FMV(32, r, T.FLOAD(32, T.REG(wordTy, C.rsp), ())),
		 T.FMV(32, fr, T.FREG(32, r))
		]
	    end

	val stms = List.concat [
		   [T.EXT(AMD64InstrExt.PUSHQ(T.REG(64, C.rbp))),
		    T.COPY (wordTy, [C.rbp], [C.rsp])],		   
		   initStms,
		   callseq, 
(*		    mv(), *)
		   [T.EXT(AMD64InstrExt.LEAVE)],
		   [T.RET []]]

        val stream as AMD64Stream.STREAM
           { beginCluster,  (* start a cluster *)
             endCluster,    (* end a cluster *)
             emit,          (* emit MLTREE stm *)
             defineLabel,   (* define a local label *)
             entryLabel,    (* define an external entry *)
             exitBlock,     (* mark the end of a procedure *)
             pseudoOp,      (* emit a pseudo op *)
             annotation,    (* add an annotation *)
             ... } =
             AMD64.selectInstructions insnStrm
	fun doit () = (
	    beginCluster 0;      (* start a new cluster *)
            pseudoOp PseudoOpsBasisTyp.TEXT;		  
	    pseudoOp (PseudoOpsBasisTyp.EXPORT [functionName]);    
            entryLabel functionName; (* define the entry label *)
            List.app emit stms; (* emit all the statements *)
            exitBlock (T.FPR (T.FREG (32, fr)) :: T.GPR (T.REG (32, C.rax)) :: result);
            endCluster [])
	val cfg = doit ()
	val cfg = AMD64RA.run cfg
	val cfg = AMD64Expand.run cfg
        in  
         (cfg, stream)        (* end the cluster *)
       end (* codegen *)

    fun dumpOutput (cfg, stream) = let
	val (cfg as Graph.GRAPH graph, blocks) = 
		AMD64BlockPlacement.blockPlacement cfg
	val CFG.INFO{annotations=an, data, decls, ...} = #graph_info graph
	in
	  AMD64Emit.asmEmit (cfg, blocks)
	end (* dumpOutput *)

  end
