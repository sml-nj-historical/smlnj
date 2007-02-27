(* amd64-svid.sml
 *
 * 
 *)

functor AMD64GenCCallFn (
  structure T : MLTREE
) = struct

  structure CTy = CTypes

  fun isFloat (CTy.C_float | CTy.C_double | CTy.C_long_double) = true
    | isFloat _ = false

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
  fun sizeOfStruct tys = let
      fun ssz ([], maxAlign, offset) =
	  {sz = alignAddr(offset, maxAlign), align = maxAlign}
	| ssz (ty::tys, maxAlign, offset) = let
	      val {sz, align} = sizeOfTy ty
	      val offset = alignAddr(offset, align)
	  in
	      ssz (tys, Int.max(maxAlign, align), offset+sz)
	  end
  in
      ssz (tys, 1, 0)
  end
			 
  (* the size alignment of a union type is the maximum of the sizes and alignments of the
   * members.  The final size is padded to agree with the alignment.
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
    | sizeOfTy CTy.C_PTR = {sz = 4, align = 4}
      | sizeOfTy (CTy.C_ARRAY(ty, n)) = let
		val {sz, align} = sizeOfTy ty
	    in
		{sz = n*sz, align = align}
	    end
    | sizeOfTy (CTy.C_STRUCT tys) = sizeOfStruct tys
    | sizeOfTy (CTy.C_UNION tys) = sizeOfUnion tys 

  val ty = 64

  structure AMD64SVID = struct

    structure C = AMD64Cells
		  
    structure TargetLang = struct
      datatype location_kind = datatype CTy.c_type
    end (* TargetLang *)
			    
    structure StagedAllocation = StagedAllocationFn (
				   structure T = T
				   structure TargetLang = TargetLang)
    structure SA = StagedAllocation
		   
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
    val intParamRegs = [rdi, rsi, rdx, rcx, r8, r9]
    val intRetRegs = [rax, rdx]
		
    fun genAutomaton () =
	let (* parameter passing *)
	    val cStack = SA.freshCounter ()
	    val cIntP = SA.freshCounter ()
	    val cFloatP = SA.freshCounter ()
	    val params = [
	        SA.WIDEN (fn w => Int.max (ty, w)),
		SA.BITCOUNTER cFloatP,
		SA.BITCOUNTER cIntP,
		SA.CHOICE [
		(fn (w, k, str) => isFloat k,
		 SA.REGS_BY_BITS (cFloatP, floatParamRegs)),
		(fn (w, k, str) => true,
		 SA.REGS_BY_BITS (cIntP, intParamRegs))],
		SA.OVERFLOW {counter=cStack,
			     blockDirection=SA.UP, maxAlign=4}
	    ]
	    (* return value *)
	    val (cFloatR, ssFloat) = SA.useRegs floatRetRegs
	    val (cIntR, ssInt) = SA.useRegs intRetRegs
	    val results = [
		SA.CHOICE [
	        (fn (w, k, str) => isFloat k,
		 SA.SEQ [SA.WIDEN (fn w => Int.max (80, w)), ssFloat]),
		(fn (w, k, str) => true,
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
  val i2s = Int.toString
  fun println s = print (s^"\n")

  fun cTyToLoc p =
      let val {sz, align} = sizeOfTy p
      in
	  (sz * 8, p, align)
      end (* cTyToLoc *)

  fun doParams ({s0, step}, callStackAddr, args, paramTys) =
      let fun offBlock i = T.ADD (ty, callStackAddr, litFromInt i)

	  fun store (rArg, paramTy as (CTy.C_signed _ | CTy.C_unsigned _ |
				      CTy.C_PTR), 
		     (str, (cs as (rcArgs, rcParams), fcs as (fcArgs, fcParams), 
		            mvs, fmvs))) =
	      let val (str', rParam) = step (str, cTyToLoc paramTy)
		  fun store' (rArg, rParam) =
		      (case (rArg, rParam)
			of (T.REG (_, regArg), [(ty, SA.REG (_, rParam), k)]) => 
			   ((regArg :: rcArgs, rParam :: rcParams), fcs, mvs, fmvs)
			 | (rArg, [(ty, SA.REG (_, rParamReg), k)]) => 
			   (cs, fcs, move (ty, rParamReg, rArg) :: mvs, fmvs)
			 | (rArg, [(ty, SA.BLOCK_OFFSET i, k)]) => 
			   (cs, fcs, T.STORE (ty, offBlock i, rArg, stack) :: 
				     mvs, fmvs)
			 | (rArg, [(ty, SA.NARROW (l, w, k), _)]) =>
(* FIXME *)
			   store' (rArg, [(w, l, k)])
		  (* esac *))
	      in
		  (str', store' (rArg, rParam))
	      end

	  val (_,((rArgs, rParams), (fArgs, fParams), mvs, fmvs)) = 
	      ListPair.foldr store (s0,(([],[]),([],[]),[],[])) (args, paramTys)
      in 
	  List.concat [
  	    [T.COPY (ty, rParams, rArgs), T.FCOPY (80, fParams, fArgs)],
	    mvs, fmvs
	  ]
      end (* doParams *)

  fun doRet ({s0, step}, retTy) =
      let fun load (str, retTy, mvs, fmvs) =
	      let fun load' (str, l) =
		      let val r = AMD64Cells.newReg ()
		      in
			  (case l
			    of [(ty, SA.REG (_, retReg), CTy.C_float)] =>
			       raise Fail "todo"
			     | [(ty, SA.REG (_, retReg), k)] =>
			       (str, move (ty, r, T.REG (ty, retReg)) :: mvs, 
				fmvs, [r])
			     (* FIXME *)
			     | [(ty, SA.NARROW (l, w, k), _)] =>
			       load' (str, [(w, l, k)]) 
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
			     
  fun genCCall {f, callStackAddr, args, proto={conv, retTy, paramTys}} = 
      let val {processParams, processReturn} = AMD64SVID.genAutomaton ()
	  val paramStms = doParams (processParams, callStackAddr, args, paramTys)

	  val {retStms, retRegs} = doRet (processReturn, retTy)
	  fun gpr r = T.GPR (T.REG (ty, r))
      in
	  { stms=List.concat [
	    paramStms,
	    [T.CALL {funct=f, targets=[], defs=[],
		     uses=map (gpr o #2) AMD64SVID.intParamRegs, 
		     region=memory, pops=0}],
	    retStms
(*	    ,[T.LIVE (map gpr retRegs)]*)
	    ],
	    retRegs=retRegs }
      end (* genCCall *)

end (* AMD64GenCCallFn *)

