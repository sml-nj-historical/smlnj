(* alpha32Props.sml
 *
 * COPYRIGHT (c) 1996 Bell Laboratories.
 *
 *)

functor Alpha32Props 
  (structure Alpha32Instr : ALPHA32INSTR
   val exnptrR : int list) : INSN_PROPERTIES =
struct
    structure I = Alpha32Instr
    structure C = I.C
    structure LE = LabelExp

    fun error msg = MLRiscErrorMsg.impossible ("alpha32Props."^msg)

    val zeroR = 31

    datatype kind = IK_JUMP | IK_NOP | IK_INSTR
    datatype target = LABELLED of Label.label | FALLTHROUGH | ESCAPES

    fun instrKind(I.BRANCH _)  = IK_JUMP
      | instrKind(I.FBRANCH _) = IK_JUMP
      | instrKind(I.JMPL _)    = IK_JUMP
      | instrKind _            = IK_INSTR

    fun branchTargets(I.BRANCH(I.BR, _, lab)) = [LABELLED lab]
      | branchTargets(I.BRANCH(_, _, lab))  = [LABELLED lab, FALLTHROUGH] 
      | branchTargets(I.FBRANCH(_, _, lab)) = [LABELLED lab, FALLTHROUGH] 
      | branchTargets(I.JMPL(_,[]))       = [ESCAPES]
      | branchTargets(I.JMPL(_,labs))     = map LABELLED labs
      | branchTargets _ = error "branchTargets"

    fun moveTmpR(I.COPY{tmp=SOME(I.Direct r), ...}) = SOME r
      | moveTmpR(I.FCOPY{tmp=SOME(I.FDirect f), ...}) = SOME f
      | moveTmpR _ = NONE

    fun moveDstSrc(I.COPY{dst, src, ...}) = (dst, src)
      | moveDstSrc(I.FCOPY{dst, src, ...}) = (dst, src)
      | moveDstSrc _ = error "moveDstSrc"

    fun moveInstr(I.COPY _) = true
      | moveInstr(I.FCOPY _) = true

(* Technically these are valid move instructions however they are
   not emitted by the code generator when emitting a move.
   I expect them to occur rarely so don't bother with them.
      | moveInstr(I.FOPERATE{oper=I.CPYS, fa, fb, ...})       = fa = fb
      | moveInstr(I.OPERATE{oper=I.BIS, rb=I.REGop 31, ...})  = true
      | moveInstr(I.ADDL(_,I.IMMop 0,_))    = true
      | moveInstr(I.BIS(31,_,_))            = true
      | moveInstr(I.SUBL(_,I.IMMop 0, _))   = true
      | moveInstr(I.ADDL(_,I.REGop 31,_))   = true
      | moveInstr(I.SUBL(_,I.REGop 31,_))   = true
      | moveInstr(I.LDA{d=I.IMMop 0, ...})  = true
      | moveInstr(I.LDAH{d=I.IMMop 0, ...}) = true
*)
      | moveInstr _			  = false

    val nop = 
      fn () => I.OPERATE{oper=I.BIS, ra=zeroR, rb=I.REGop zeroR, rc=zeroR}

    (* Resource usage *)
    fun defUseR instr =
      let
	fun Oper {oper, ra, rb=I.REGop rb, rc} = ([rc], [ra, rb])
	  | Oper {oper, ra, rb, rc} = ([rc], [ra])
	fun FMem (freg, (rd, _)) = ([], [rd])
	fun trap (def,use) =(def, exnptrR @ use)
      in
	case instr of
	  (* load/store instructions *)
	   I.LDA{r, b, ...} => ([r], [b])
	 | I.LDAH{r, b, ...} => ([r], [b])
	 | I.LOAD{r, b, ...} => ([r], [b])
         | I.STORE{r, b, ...} => ([], [r,b])
	 | I.FLOAD{b, ...} => ([], [b])
	 | I.FSTORE{b, ...} => ([], [b])
	 (* branch instructions *)
	 | I.JMPL ({r, b, ...},_) => ([r], [b])
	 | I.JSR({r, b, ...}, def, use) => (r:: #1 def, b:: #1 use)
	 | I.BRANCH(I.BR, reg, _) => ([reg], [])
	 | I.BRANCH(_, reg, _) => ([], [reg])
	 (* operate *)
	 | I.OPERATE arg => Oper arg
	 | I.PSEUDOARITH {oper, ra, rb=I.REGop rb, rc, tmps} => 
	     (rc:: #1 tmps, [ra, rb])
	 | I.PSEUDOARITH {oper, ra, rb, rc, tmps} => (rc:: #1 tmps, [ra])
	 | I.OPERATEV arg => trap(Oper arg)
	 (* copy *)
	 | I.COPY{dst, src, tmp=SOME(I.Direct r), ...} => (r::dst, src)
	 | I.COPY{dst, src, ...} => (dst, src)
	 (* floating operate *)
	 | I.FOPERATEV _ => trap([], [])
	 | I.TRAPB 	=> trap([],[])
	 (* macro *)
	 | I.CALL_PAL{def,use, ...} => (def, use)

	 | _  		=> ([],[])
      end

    (* Use of FP registers *)
    fun defUseF instr =
      case instr of
	I.DEFFREG freg				=> ([freg], [])
      | I.FBRANCH(_, freg, lab)			=>  ([],[freg])
      | I.FLOAD{r, ...}				=> ([r], [])
      | I.FSTORE{r, ...}			=> ([], [r])
      | I.FOPERATE{fa, fb, fc, ...}		=> ([fc], [fa, fb])
      | I.PSEUDOARITH{tmps, ...}		=> (#2 tmps, [])
      | I.FOPERATEV{oper=I.CVTTQ, fa, fb, fc}   => ([fc], [fa, fb])
      | I.FOPERATEV{fa, fb, fc, ...}		=> ([fc], [fa, fb, fc])
      | I.FCOPY{dst, src, tmp=SOME(I.FDirect f), ...} => (f::dst, src)
      | I.FCOPY{dst, src, ...}			=> (dst, src) 
      | I.JSR(_,def,use) 	     => (#2 def,#2 use)
      | _ => ([],[])


    (* These numbers are true of the DECchip 21064-AA implementation. *)
    (* Load class *)
    fun latency(I.LOAD _)       = 5
      | latency(I.FLOAD _)      = 5

      | latency(I.OPERATE{oper, ...}) = 
         (case oper
          of I.SRA	=> 2
	   | I.SRL	=> 2
	   | I.SLL	=> 2
	   | I.INSBL	=> 2
	   | I.EXTBL	=> 2
	   | I.EXTQH	=> 2
	   | I.MSKBL	=> 2
	   | I.MSKLH	=> 2
	   | I.CMPULE   => 3
	   | I.CMPULT   => 3
	   | I.CMPEQ	=> 3
	   | I.CMPLE	=> 3
	   | I.CMPLT	=> 3
	   | I.MULL  	=> 21
  	   | _ => 1
	  (*esac*))
      | latency (I.OPERATEV{oper=I.MULLV, ...}) = 21

      (* Floating point *)
      | latency(I.FOPERATEV{oper=I.DIVT, ...}) = 63
      | latency(I.FOPERATEV _) = 6
      | latency(I.FOPERATE _)  = 6

      | latency(I.CALL_PAL _)   = 30

      | latency _ 		= 1
end



(*
 * $Log: alpha32Props.sml,v $
 * Revision 1.3  1998/02/16 13:57:44  george
 *   A register allocated temp is now associated with parallel COPYs
 *   instead of a dedicated register. The temp is used to break cycles.
 *
 * Revision 1.2  1997/08/29 11:00:06  george
 *   Added code to handle the new LDS, CVTLQ, DIVL and DIVLU instructions.
 *
# Revision 1.1.1.1  1997/04/19  18:14:22  george
#   Version 109.27
#
 *)
