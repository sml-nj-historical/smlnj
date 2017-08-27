(* mkRecord.sml
 *
 * COPYRIGHT (c) 2017 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * TODO:
 *   Some CPS.RECORDs can be created using a tight loop implementing
 *   a block copy.
 *
 * NOTE: this file does not appear to be used anywhere, so it should
 * probably go away!
 *)

functor MkRecord (

    structure MS: MACH_SPEC
    structure C: CPSREGS where T.Region = CPSRegions

) : MK_RECORD = struct

  structure T = C.T
  structure R = CPSRegions

  fun error msg = ErrorMsg.impossible ("MkRecord." ^ msg)

  val addrTy = MS.addressBitWidth
  val pty = MS.wordBitWidth
  val ity = MS.wordBitWidth
  val fty = 64
  val wordSz = MS.wordByteWidth

  fun ea(r, 0) = r
    | ea(r, n) = T.ADD(addrTy, r, T.LI n)

  fun indexEA(r, 0) = r
    | indexEA(r, n) = T.ADD(addrTy, r, T.LI(n*wordSz))

  fun pi(x as ref(R.PT.TOP _),_) = x
    | pi(x,i) = R.PT.pi(x,i)

  fun record {desc, fields, mem, hp, emit, markPTR, markComp} = let
    fun getfield(r, CPS.SELp(n, p), mem) = 
        let val mem = pi(mem,n)
        in  getfield(markPTR(T.LOAD(ity, indexEA(r, n), mem)), p, mem) end
      | getfield(r, CPS.OFFp 0, _) = r
      | getfield(r, CPS.OFFp n, _) = T.ADD(addrTy, r, T.LI(n*wordSz))

    fun storeFields ([], _, _, _) = ()
      | storeFields ((v, p)::rest, n, mem, i) = 
        let val elem = pi(mem, i)
        in  emit(T.STORE(ity, T.ADD(addrTy, C.allocptr, T.LI n), 
                 getfield(v, p, elem), elem));
	    storeFields(rest, n + wordSz, mem, i+1)
        end
  in
    emit(T.STORE(ity, ea(C.allocptr, hp), desc, pi(mem,~1)));
    storeFields(fields, hp+wordSz, mem, 0);
    hp + wordSz
  end

  fun frecord {desc, fields, mem, hp, emit, markPTR, markComp} = let
    fun fgetfield(T.FPR fp, CPS.OFFp 0, _) = fp
      | fgetfield(T.GPR r, path, mem) = let
	  fun fea(r, 0) = r
	    | fea(r, n) = T.ADD(addrTy, r, T.LI(n*8))

	  fun chase(r, CPS.SELp(n, CPS.OFFp 0), mem) =
		markComp(T.FLOAD(fty, fea(r,n), pi(mem,n)))
	    | chase(r, CPS.SELp(n,p), mem) =  
              let val mem = pi(mem,n)
              in  chase(markPTR(T.LOAD(ity, indexEA(r, n), mem)), p, mem) 
              end
	in chase(r, path, mem)
	end

    fun fstoreFields ([], _, _, _) = ()
      | fstoreFields ((v, p)::rest, n, mem, i) =
        let val elem = pi(mem, i)
        in  emit(T.FSTORE(fty, T.ADD(addrTy, C.allocptr, T.LI n),
                          fgetfield(v, p, elem), elem));
	    fstoreFields(rest, n + 8, mem, i + 8 div wordSz)
        end
  in
    emit(T.STORE(ity, ea(C.allocptr, hp), desc, pi(mem,~1)));
    fstoreFields(fields, hp+wordSz, mem, 0);
    hp + wordSz
  end	
end

