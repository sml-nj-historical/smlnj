(* mk-record.sml --- translate a CPS.RECORD to MLRISC
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

(* TODO:
 *   Some CPS.RECORDs can be created using a tight loop implementing
 *   a block copy.
 *)

functor MkRecord(C: CPSREGS where T.Region = CPSRegions) : MK_RECORD =
struct
  structure T = C.T
  structure R = CPSRegions

  fun error msg = ErrorMsg.impossible ("MkRecord." ^ msg)

  val addrTy = C.addressWidth
  val pty = 32
  val ity = 32
  val fty = 64

  fun ea(r, 0) = r
    | ea(r, n) = T.ADD(addrTy, r, T.LI n)

  fun indexEA(r, 0) = r
    | indexEA(r, n) = T.ADD(addrTy, r, T.LI(n*4))

  fun pi(x as ref(R.PT.TOP _),_) = x
    | pi(x,i) = R.PT.pi(x,i)

  fun record {desc, fields, mem, hp, emit, markPTR, markComp} = let
    fun getfield(r, CPS.SELp(n, CPS.OFFp 0), mem) = 
        let val mem = pi(mem,n)
        in  markComp(T.LOAD(ity, indexEA(r, n), mem)) end
      | getfield(r, CPS.SELp(n, CPS.OFFp off), mem) = 
        let val mem = pi(mem,n)
        in  T.ADD(addrTy,markComp(T.LOAD(ity, indexEA(r, n), mem)),T.LI(off+4))
        end
      | getfield(r, CPS.SELp(n, p), mem) = 
        let val mem = pi(mem,n)
        in  getfield(markPTR(T.LOAD(ity, indexEA(r, n), mem)), p, mem) end
      | getfield(r, CPS.OFFp 0, _) = r
      | getfield(r, CPS.OFFp n, _) = T.ADD(addrTy, r, T.LI(n*4))

    fun storeFields ([], _, _, _) = ()
      | storeFields ((v, p)::rest, n, mem, i) = 
	 (emit(T.STORE(ity, T.ADD(addrTy, C.allocptr, T.LI n), 
               getfield(v,p,mem), pi(mem,i)));
	  storeFields(rest, n + 4, mem, i+1))
  in
    emit(T.STORE(ity, ea(C.allocptr, hp), desc, pi(mem,~1)));
    storeFields(fields, hp+4, mem, 0);
    hp + 4
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
	  (emit(T.FSTORE(fty, 
                T.ADD(addrTy,C.allocptr,T.LI n),fgetfield(v,p,mem), pi(mem,i)));
	   fstoreFields(rest, n + 8, mem, i+2))
  in
    emit(T.STORE(ity, ea(C.allocptr, hp), desc, pi(mem,~1)));
    fstoreFields(fields, hp+4, mem, 0);
    hp + 4
  end	
end

