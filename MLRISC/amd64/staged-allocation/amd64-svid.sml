(* amd64-svid.sml
 *
 * 
 *)

functor AMD64GenCCallFn (
  structure T : MLTREE
) : C_CALL = struct

  structure T = T

  datatype c_arg = datatype T.mlrisc

  structure CTy = CTypes

  val ty = 64

  fun gpr r = T.GPR (T.REG (ty, r))
  fun fpr(sz,f) = T.FPR (T.FREG (sz, f))
  val fpStk = List.tabulate(8, fn i => fpr (80, AMD64Cells.ST i))
		  
  fun isFloat (CTy.C_float | CTy.C_double | CTy.C_long_double) = true
    | isFloat _ = false

  fun isMem (CTy.C_STRUCT _ | CTy.C_UNION _) = true
    | isMem _ = false

  (* size and natural alignment for integer types. *)
  fun sizeOfInt CTy.I_char = {ty = 8, sz = 1, align = 1}
    | sizeOfInt CTy.I_short = {ty = 16, sz = 2, align = 2}
    | sizeOfInt CTy.I_int = {ty = 32, sz = 4, align = 4}
    | sizeOfInt CTy.I_long = {ty = 32, sz = 4, align = 4}
    | sizeOfInt CTy.I_long_long = {ty = 64, sz = 8, align = 8}
				  
  (* sizes of other C types *)
  val sizeOfPtr = {ty = 64, sz = 8, align = 8}

(* align the address to the given alignment, which must be a power of 2 *)
  fun alignAddr (addr, align) = 
      let val mask = Word.fromInt(align-1)
      in
	  Word.toIntX(Word.andb(Word.fromInt addr + mask, Word.notb mask))
      end
      
  (* compute the size and alignment information for a struct; tys is the list
   * of member types.
   * The total size is padded to agree with the struct's alignment.
   *)
  fun sizeOfStruct tys = 
      let fun ssz ([], maxAlign, offset) =
	      {sz = alignAddr(offset, maxAlign), align = maxAlign}
	    | ssz (ty::tys, maxAlign, offset) = 
	      let val {sz, align} = sizeOfTy ty
		  val offset = alignAddr(offset, align)
	      in
		  ssz (tys, Int.max(maxAlign, align), offset+sz)
	      end
      in
	  ssz (tys, 1, 0)
      end
			 
  (* the size alignment of a union type is the maximum of the sizes and alignments 
   * of the members.  The final size is padded to agree with the alignment.
   *)
  and sizeOfUnion tys = let
      fun usz ([], maxAlign, maxSz) =
	  {sz = alignAddr(maxSz, maxAlign), align = maxAlign}
	| usz (ty::tys, maxAlign, maxSz) = let
	      val {sz, align} = sizeOfTy ty
	  in
	      usz (tys, Int.max(maxAlign, align), Int.max(sz, maxSz))
	  end
  in
      usz (tys, 1, 0)
  end
		  
  and sizeOfTy CTy.C_void = raise Fail "unexpected void argument type"
    | sizeOfTy CTy.C_float = {sz = 4, align = 4}
    | sizeOfTy CTy.C_double = {sz = 8, align = 8}
    | sizeOfTy CTy.C_long_double = {sz = 12, align = 8}
    | sizeOfTy (CTy.C_unsigned isz) = let
	  val {sz, align, ...} = sizeOfInt isz
      in
	  {sz = sz, align = align}
      end
    | sizeOfTy (CTy.C_signed isz) = let
	  val {sz, align, ...} = sizeOfInt isz
      in
	  {sz = sz, align = align}
      end
    | sizeOfTy CTy.C_PTR = {sz = 8, align = 8}
      | sizeOfTy (CTy.C_ARRAY(ty, n)) = let
		val {sz, align} = sizeOfTy ty
	    in
		{sz = n*sz, align = align}
	    end
    | sizeOfTy (CTy.C_STRUCT tys) = sizeOfStruct tys
    | sizeOfTy (CTy.C_UNION tys) = sizeOfUnion tys 

  datatype c_location_kind = K_GPR
			   | K_FLOAT
			   | K_MEM

  fun kindOfCTy (CTy.C_float | CTy.C_double | CTy.C_long_double) = K_FLOAT
    | kindOfCTy (CTy.C_STRUCT _ | CTy.C_UNION _ | CTy.C_ARRAY _) = K_MEM
    | kindOfCTy _ = K_GPR

  structure AMD64SVID : CALL_CONV = struct

    structure C = AMD64Cells
		  
    structure TargetLang = struct
      datatype location_kind = datatype c_location_kind
    end (* TargetLang *)
			    
    structure StagedAllocation = StagedAllocationFn (
				   structure T = T
				   structure TargetLang = TargetLang)
    structure SA = StagedAllocation

    type reg = (int * CellsBasis.cell)
		   
    type slot = SA.slot
    type location_info = SA.location_info
    type automaton = {s0 : StagedAllocation.str, step : StagedAllocation.stepper_fn}
			 
    val [rax, rdi, rsi, rdx, rcx, r8, r9] = map (fn r => (ty, r))
	  [C.rax, C.rdi, C.rsi, C.rdx, C.rcx, C.GPReg 8, C.GPReg 9]

(* FIXME: need to add xmm* regs to amd64.mdl *)					    
    val floatParamRegs = []
    (*[xmm2, xmm3, xmm4, xmm5, xmm6, xmm7]*)
    val floatRetRegs = []
    (*[xmm0, xmm1]*)
    val gprParamRegs = [rdi, rsi, rdx, rcx, r8, r9]
    val intRetRegs = [rax, rdx]
		
    fun genAutomaton () =
	let (* parameter passing *)
	    val cStack = SA.freshCounter ()
	    val cIntP = SA.freshCounter ()
	    val cFloatP = SA.freshCounter ()
	    val params = [
		SA.CHOICE [
		(fn (w, k, str) => k = K_FLOAT,
		 SA.SEQ [
		 SA.BITCOUNTER cFloatP,
		 SA.REGS_BY_BITS (cFloatP, floatParamRegs)]),
		(fn (w, k, str) => k = K_MEM, 
		 SA.OVERFLOW {counter=cStack,
			      blockDirection=SA.UP, maxAlign=16}),
		(fn (w, k, str) => k = K_GPR,
		 SA.SEQ [
	         SA.WIDEN (fn w => Int.max (ty, w)),
		 SA.BITCOUNTER cIntP,
		 SA.REGS_BY_BITS (cIntP, gprParamRegs)])
		],
		SA.OVERFLOW {counter=cStack,
			     blockDirection=SA.UP, maxAlign=16}
	    ]
	    (* return value *)
	    val (cFloatR, ssFloat) = SA.useRegs floatRetRegs
	    val (cIntR, ssInt) = SA.useRegs intRetRegs
	    val results = [
		SA.CHOICE [
	        (fn (w, k, str) => k = K_FLOAT,
		 SA.SEQ [SA.WIDEN (fn w => Int.max (80, w)), ssFloat]),
		(fn (w, k, str) => k = K_GPR,
		 SA.SEQ [SA.WIDEN (fn w => Int.max (ty, w)), ssInt])
		]
	    ]
			  
	    val processParams = {s0=SA.init [cStack, cFloatP, cIntP], 
				 step=SA.mkStep params}
	    val processReturn = {s0=SA.init [cFloatR, cIntR],
				 step=SA.mkStep results}
	in
	    {processParams=processParams, processReturn=processReturn}
	end (* genAutomaton *)
	
  end (* AMD64SVID *)

  structure CallConv = AMD64SVID
  structure SA = CallConv.StagedAllocation
  structure MD = CallConv.TargetLang

  val memory = T.Region.memory
  val stack = T.Region.stack
  fun litFromInt i = T.LI (T.I.fromInt (ty, i))
  fun move (ty, r, e) = T.MV (ty, r, e)
  fun fmove (fty, r, e) = T.FMV (fty, r, e)

  (* debugging cruft *)
  val i2s = Int.toString
  fun println s = print (s^"\n")

  fun szToLoc cty {sz, align} = (sz * 8, kindOfCTy cty, align)
  fun cTyToLoc cty = szToLoc cty (sizeOfTy cty)
  fun unNarrow (w, SA.NARROW (l, _, _), k) = (w, l, k)
    | unNarrow l = l

  fun structLocs structTys =
      let val {sz, align} = sizeOfStruct structTys
	  fun szOfField (cty, sz) = (#sz (sizeOfTy cty)) + sz
	  val szOfFields = foldl szOfField 0
(*	  fun classify ctys =
	      if szOfFields > 16 then K_MEM
	      else*)
      in 
raise Fail "todo"
      end (* structLocs *)

  fun doParams ({s0, step}, callStackAddr, args, paramTys) =
      let fun offBlock i = T.ADD (ty, callStackAddr, litFromInt i)

	  fun copyGPR (rArg, rParam, env as 
		  {str, cs=cs as (rcArgs, rcParams), 
		   fcs=fcs as (fcArgs, fcParams), mvs, fmvs}) =
	      (case (rArg, rParam)
		of (T.REG (_, regArg), (ty, SA.REG (_, rParamReg), _)) => 
		   {str=str, cs=(regArg :: rcArgs, rParamReg :: rcParams), fcs=fcs, 
		    mvs=mvs, fmvs=fmvs}
		 | (rArg, (ty, SA.REG (_, rParamReg), _)) => 
		   {str=str, cs=cs, fcs=fcs, mvs=move (ty, rParamReg, rArg) :: mvs, 
		    fmvs=fmvs}
		 | (rArg, (ty, SA.BLOCK_OFFSET i, _)) => 
		   {str=str, cs=cs, fcs=fcs, 
		    mvs=T.STORE (ty, offBlock i, rArg, stack) :: mvs, fmvs=fmvs}
		 | (rArg, (_, SA.NARROW (l, w, k), _)) =>
		   copyGPR (rArg, (w, l, k), env) 
	      (* esac *))

	  fun copyStruct (csz, rArg, structTys, {str, cs, fcs, mvs, fmvs}) =
	      let (* loadAddr addresses the struct *)
		  val (loadAddr, addrR) = 
		      (case rArg
			of T.REG (_, r) => ([], T.REG (ty, r))
			 | _ => let val r = AMD64Cells.newReg ()
			       in
				   ([move (ty, r, rArg)], T.REG (ty, r))
			       end
		      (* esac *))
		  fun offAddr 0 = addrR
		    | offAddr offset = T.ADD (ty, addrR, T.LI offset)
		  val sz = szToLoc (CTy.C_STRUCT structTys)
		  val (str', l) = step (str, sz (sizeOfStruct structTys)) 
		  val (w, SA.BLOCK_OFFSET blk, _) = unNarrow l
		  fun doStore (_, [], mvs) = mvs
		    | doStore (i, (cty as (CTy.C_unsigned _ | 
				  CTy.C_signed _)) :: paramTys, mvs) =
		      let val ty = sizeOfTy cty
		      in
			  doStore (i+(#align ty),
			     paramTys, T.STORE (#sz ty*8, offBlock i, rArg, stack) :: mvs)
		      end
	      in
		  {str=str', cs=cs, fcs=fcs, 
		   mvs=doStore (blk, structTys, mvs) , fmvs=fmvs}
	      end 

	  and copyArgs (rArg, paramTy, env as {str, cs, fcs, mvs, fmvs}) =
	      (case (rArg, kindOfCTy paramTy, paramTy)
		of (T.GPR e, K_GPR, _) =>
		   let val (str', rParam) = step (str, cTyToLoc paramTy)
		   in
		       copyGPR (e, rParam, {str=str', 
			       cs=cs,fcs=fcs,mvs=mvs,fmvs=fmvs})
		   end
		 | (T.GPR ptr, K_MEM, CTy.C_STRUCT structTys) =>
		   let val csz = sizeOfTy paramTy
		   in
		       if #sz csz < 16
		       then raise Fail "todo"
		       else copyStruct (csz, ptr, structTys, env)
		   end
		 | (T.FPR e, K_FLOAT, _) => raise Fail "todo"
		 | _ => raise Fail "copyArgs"
	      (* esac *))

	  val {str,cs=(rArgs, rParams), fcs=(fArgs, fParams), mvs, fmvs} = 
	      ListPair.foldl copyArgs 
		     {str=s0, cs=([],[]), fcs=([],[]), mvs=[], fmvs=[]}
		     (args, paramTys)
      in 
	  List.concat [
    	  [T.COPY (ty, rParams, rArgs), T.FCOPY (80, fParams, fArgs)], mvs, fmvs]
      end (* doParams *)

  fun doRet ({s0, step}, retTy) =
      let fun load (str, retTy, mvs, fmvs) =
	      let fun load' (str, l) =
		      let val r = AMD64Cells.newReg ()
		      in
			  (case l
			    of (ty, SA.REG (_, retReg), K_FLOAT) =>
			       raise Fail "todo"
			     | (ty, SA.REG (_, retReg), k) =>
			       (str, move (ty, r, T.REG (ty, retReg)) :: mvs, 
				fmvs, [T.GPR (T.REG (ty, r))])
			     | (_, SA.NARROW (l, w, k), _) =>
			       load' (str, (w, l, k)) 
			  (* esac *))
		      end
	      in
		  load' (step (str, cTyToLoc retTy))
	      end 
	  val (_, mvs, fmvs, rs) = load (s0, retTy, [], [])

      in
	  {retStms=List.concat [
	   mvs, fmvs
	   ], 
	   retRegs=rs}
      end (* doRet *)

  val callerSaveRegs = [AMD64Cells.rax, AMD64Cells.rcx, AMD64Cells.rdx,
			AMD64Cells.rsi, AMD64Cells.rdi, 
			AMD64Cells.GPReg 8, AMD64Cells.GPReg 9, 
			AMD64Cells.GPReg 10, AMD64Cells.GPReg 11]
		       			     
  fun genCall {name, proto={conv, retTy, paramTys}, args} =
      let val callStackAddr = T.REG (ty, AMD64Cells.rsp)
	  val {processParams, processReturn} = AMD64SVID.genAutomaton ()
	  val paramStms = doParams (processParams, callStackAddr, args, paramTys)

	  val {retStms, retRegs} = doRet (processReturn, retTy)
      in
	  { callseq=List.concat [
	    paramStms,
	    [T.CALL {funct=name, targets=[], defs=map gpr callerSaveRegs
						  @ fpStk,
		     uses=[] (*map (gpr o #2) AMD64SVID.gprParamRegs*), 
		     region=memory, pops=0}],
	    retStms
	    ],
	    result=retRegs }
      end (* genCCall *)

  val calleeSaveRegs = []
  val calleeSaveFRegs = []

end (* AMD64GenCCallFn *)

