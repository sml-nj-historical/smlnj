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

  val pty = 32
  val ity = 32
  val fty = 64

  val T.REG(pty,allocptrR) = C.allocptr

  fun ea(r, 0) = r
    | ea(r, n) = T.ADD(pty, r, T.LI n)

  fun indexEA(r, 0) = r
    | indexEA(r, n) = T.ADD(pty, r, T.LI(n*4))

  fun pi(x as ref(R.PT.TOP _),_) = x
    | pi(x as ref(R.PT.NAMED _),_) = x
    | pi(x,i) = R.PT.pi(x,i)

  fun record {desc, fields, ans, mem, hp, emit} = let
    fun getfield(r, CPS.SELp(n, p), mem) = 
        let val mem = pi(mem,n)
        in  getfield(T.LOAD(ity, indexEA(r, n), mem), p, mem) end
      | getfield(r, CPS.OFFp 0, _) = r
      | getfield(r, CPS.OFFp n, _) = T.ADD(pty, r, T.LI(n*4))

    fun storeFields ([], _, _, _) = ()
      | storeFields ((v, p)::rest, n, mem, i) = 
	 (emit(T.STORE(ity, T.ADD(pty, C.allocptr, T.LI n), 
               getfield(v,p,mem), pi(mem,i)));
	  storeFields(rest, n + 4, mem, i+1))
  in
    emit(T.STORE(ity, ea(C.allocptr, hp), desc, pi(mem,~1)));
    storeFields(fields, hp+4, mem, 0);
    emit(T.MV(pty, ans, T.ADD(pty, C.allocptr, T.LI(hp+4))))
  end

  fun frecord {desc, fields, ans, mem, hp, emit} = let
    fun fgetfield(T.FPR fp, CPS.OFFp 0, _) = fp
      | fgetfield(T.GPR r, path, mem) = let
	  fun fea(r, 0) = r
	    | fea(r, n) = T.ADD(pty, r, T.LI(n*8))

	  fun chase(r, CPS.SELp(n, CPS.OFFp 0), mem) =
		T.FLOAD(fty, fea(r,n), pi(mem,n))
	    | chase(r, CPS.SELp(n,p), mem) =  
              let val mem = pi(mem,n)
              in  chase(T.LOAD(ity, indexEA(r, n), mem), p, mem) end
	in chase(r, path, mem)
	end

    fun fstoreFields ([], _, _, _) = ()
      | fstoreFields ((v, p)::rest, n, mem, i) =
	  (emit(T.FSTORE(fty, 
                T.ADD(pty, C.allocptr, T.LI n),fgetfield(v,p,mem), pi(mem,i)));
	   fstoreFields(rest, n + 8, mem, i+2))
  in
    emit(T.STORE(ity, ea(C.allocptr, hp), desc, pi(mem,~1)));
    fstoreFields(fields, hp+4, mem, 0);
    emit(T.MV(pty, ans, T.ADD(pty, C.allocptr, T.LI(hp+4))))
  end	
end

(*
 * $Log: mkRecord.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:54  george
 * Version 110.5
 *
 *)
