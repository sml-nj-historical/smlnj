(* vlBackPatch.sml -- variable length back patching. 
 *
 * Copyright 1999 by Bell Laboratories 
 *)
signature MC_EMIT = sig
  structure I : INSTRUCTIONS

  val emitInstr : I.instruction -> Word8Vector.vector
end

  
functor BackPatch
  (structure CodeString : CODE_STRING
   structure Jumps      : SDI_JUMPS 
   structure Props      : INSN_PROPERTIES 
			where I = Jumps.I
   structure Emitter    : MC_EMIT
			where I = Props.I
   structure CFG        : CONTROL_FLOW_GRAPH
			where I = Emitter.I
   structure Asm	: INSTRUCTION_EMITTER
			where I = CFG.I
   structure Placement  : BLOCK_PLACEMENT
			where CFG = CFG)  : BBSCHED = 
struct 
  structure I   = Jumps.I
  structure C   = I.C
  structure CFG = CFG
  structure P   = CFG.P
  structure G   = Graph
  structure W8V = Word8Vector

  datatype desc =
      BYTES of W8V.vector * desc 
    | PSEUDO of P.pseudo_op  * desc
    | SDI of I.instruction * int ref * desc
    | LABEL of Label.label * desc
    | NIL

  datatype cluster = CLUSTER of {cluster: desc}

  fun error msg = MLRiscErrorMsg.error("vlBackPatch",msg)

  val clusters = ref ([] : cluster list)

  fun cleanUp() = clusters := []

  val Asm.S.STREAM{emit,...} = Asm.makeStream []

  fun bbsched(cfg as G.GRAPH graph) = let
    val blocks = map #2 (Placement.blockPlacement cfg)
    fun bytes([], p) = p
      | bytes([s], p) = BYTES(s, p)
      | bytes(s, p) = BYTES(W8V.concat s, p)

    fun f(CFG.BLOCK{data, labels, insns, ...}::rest) = let
	 fun instrs([], b) = bytes(rev b, f rest)
	   | instrs(i::rest, b) = 
	     if Jumps.isSdi i then 
	       bytes(rev b, SDI(i, ref(Jumps.minSize i), instrs(rest, [])))
	     else
	       instrs(rest, Emitter.emitInstr(i)::b)
	 fun doLabels(lab::rest) = LABEL(lab, doLabels rest)
	   | doLabels [] = instrs(rev(!insns), [])
	 fun pseudo(CFG.PSEUDO pOp :: rest) = PSEUDO(pOp, pseudo rest)
	   | pseudo(CFG.LABEL lab :: rest) =  LABEL(lab, pseudo rest)
	   | pseudo [] = doLabels(!labels)

	in pseudo(!data)
	end 
      | f [] = NIL
  in 
    clusters := 
      CLUSTER{cluster=f blocks}:: !clusters
  end


  fun finish() = let
    fun labels (BYTES(s,rest), pos, chgd) = labels(rest, pos+W8V.length s, chgd)
      | labels (SDI(_, ref size, rest), pos, chgd) = labels(rest, pos+size, chgd)
      | labels (LABEL(l,rest), pos, changed) = 
        if Label.addrOf(l) = pos then labels(rest, pos, changed)
	else (Label.setAddr(l, pos); labels(rest, pos, true))
      | labels (PSEUDO(pOp, rest), pos, chgd) = let
	  val oldSz = P.sizeOf(pOp, pos)
	  val newSz = (P.adjustLabels(pOp, pos); P.sizeOf(pOp, pos))
        in labels(rest, pos + newSz, chgd orelse newSz<>oldSz)
	end
      | labels (NIL, pos, chgd) = (pos, chgd)

    fun clusterLabels clusters = let
      fun f (CLUSTER{cluster, ...}, (pos,chgd)) = labels(cluster, pos, chgd) 
    in List.foldl f (0, false) clusters
    end
      
    fun adjust([], pos) = () 
      | adjust(CLUSTER{cluster}::rest, pos) = let
	  fun f(pos, BYTES(s, rest)) = f(pos+W8V.length s, rest)
	    | f(pos, SDI(instr, r as ref size, rest)) = let
		val s = Jumps.sdiSize(instr, Label.addrOf, pos)
	      in
		if s > size then r := s else ();
		f (pos+size, rest)
	      end
	    | f(pos, LABEL(l,rest)) = f(pos, rest)
	    | f(pos, PSEUDO(pOp,rest)) = f(pos + P.sizeOf(pOp, pos), rest)
	    | f(pos, NIL) = adjust(rest, pos)
        in f(pos, cluster)
        end
       
    val nop = Props.nop()

    val loc = ref 0

    fun output v = 
      W8V.app (fn v => (CodeString.update(!loc, v); loc:= !loc+1)) v


    fun chunk(pos, []) = ()
      | chunk(pos, CLUSTER{cluster}::rest) = let
          fun outputInstr i = output (Emitter.emitInstr(nop))
          fun nops 0 = ()
	    | nops n = 
	       if n < 0 then error "chunk.nops"
	       else (outputInstr(nop); nops(n-1))
	  fun f(pos, BYTES(s,r)) = (output s; f(pos+W8V.length s,r))
	    | f(pos, SDI(instr, ref size, r)) = let
	        val emitInstrs = map (fn i => Emitter.emitInstr(i))
	        val instrs = emitInstrs (Jumps.expand(instr, size, pos))
		val sum = List.foldl (fn (a,b) => (W8V.length a + b)) 0
		val n = size - sum instrs
              in
		if n > 0 then 
		  (print ("\t\t\t Inserting " ^ Int.toString n ^ "nops\n");
		   emit instr)
		else ();
		app output instrs;
		if n < 0 then 
		  error "chunk: numNops"
		else 
		  if n = 0 then () else nops(n);
		f(pos+size, r)
	      end
	    | f(pos, LABEL(lab, rest)) = 
	        if pos = Label.addrOf lab then f(pos,rest)
		else error "chunk: LABEL"
	    | f(pos, PSEUDO(pOp, rest)) = let
	        val s : Word8.word list ref = ref []
	      in
	        P.emitValue{pOp=pOp, loc=pos, 
			    emit=(fn w => s :=  w :: (!s))};
		output(W8V.fromList(rev(!s)));
		f(pos + P.sizeOf(pOp, pos), rest)
              end
	    | f(pos, NIL) = chunk(pos, rest)

        in f(pos, cluster)
	end

    fun fix clusters = let
      val (pos, changed) = clusterLabels clusters
    in 
      if changed then (adjust(clusters, 0); fix clusters) else pos
    end

    val clusters = rev(!clusters) before clusters := []
  in
    CodeString.init(fix clusters);
    loc := 0; chunk(0, clusters) 
  end (* finish *)

end (* functor BackPatch *)


