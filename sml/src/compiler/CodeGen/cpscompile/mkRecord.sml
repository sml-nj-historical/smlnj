(* mk-record.sml --- translate a CPS.RECORD to MLRISC
 *
 * COPYRIGHT (c) 1996 AT&T Bell Laboratories.
 *
 *)

(* TODO:
 *   Some CPS.RECORDs can be created using a tight loop implementing
 *   a block copy.
 *)

functor MkRecord
  (structure C: CPSREGS where T.Region = CPSRegions
   structure MLTreeComp : MLTREECOMP
     sharing C.T = MLTreeComp.T) : MK_RECORD =
struct
  structure T : MLTREE = C.T
  structure R = CPSRegions

  fun error msg = ErrorMsg.impossible ("MkRecord." ^ msg)

  val emit = MLTreeComp.mlriscComp

  val dummyRegion = (R.RVAR 0, R.RO_MEM, CPS.OFFp 0)

  val T.REG allocptrR = C.allocptr

  fun ea(r, 0) = r
    | ea(r, n) = T.ADD(r, T.LI(n))

  fun indexEA(r, 0) = r
    | indexEA(r, n) = T.ADD(r, T.LI(n*4))

  fun record {desc, fields, ans, mem, hp} = let
    val descRegion::regions = 
      case mem
      of R.RO_MEM => dummyRegion:: map (fn _ => dummyRegion) fields
       | R.RECORD vl => vl

    fun getfield(r, CPS.SELp(n, p), R.RO_MEM) = 
	  getfield(T.LOAD32(indexEA(r, n), R.RO_MEM), p, R.RO_MEM)
      | getfield(r, CPS.SELp(n, p), R.RECORD vl) = let
	  val (def, root, ap) = List.nth(vl, n+1)
	in getfield(T.LOAD32(indexEA(r, n), def), p, R.trace(root, ap))
	end
      | getfield(r, CPS.SELp(n, p), R.OFFSET(i, vl)) = let
	  val (def, root, ap) = List.nth(vl, n+i+1)
	in getfield(T.LOAD32(indexEA(r, n), def), p, R.trace(root, ap))
	end
      | getfield(r, CPS.OFFp 0, _) = r
      | getfield(r, CPS.OFFp n, _) = T.ADD(r, T.LI(n*4))

    fun storeFields ([], _, []) = ()
      | storeFields ((v, p)::rest, n, (def, root, _)::regions) =
	 (emit(T.STORE32(T.ADD(C.allocptr, T.LI n), getfield(v,p,root), def));
	  storeFields(rest, n + 4, regions))
  in
    emit(T.STORE32(ea(C.allocptr, hp), desc, #1 descRegion));
    storeFields(fields, hp+4, regions);
    emit(T.MV(ans, T.ADD(C.allocptr, T.LI(hp+4))))
  end

  fun frecord {desc, fields, ans, mem, hp} = let
    val descRegion::regions =
      case mem
      of R.RO_MEM => dummyRegion:: map (fn _ => dummyRegion) fields
       | R.RECORD vl => vl

    fun fgetfield(T.FPR fp, CPS.OFFp 0, _) = fp
      | fgetfield(T.GPR r, path, mem) = let
	  fun fea(r, 0) = r
	    | fea(r, n) = T.ADD(r, T.LI(n*8))

	  fun chase(r, CPS.SELp(n, CPS.OFFp 0), R.RO_MEM) =
		T.LOADD(fea(r,n), R.RO_MEM)
	    | chase(r, CPS.SELp(n, CPS.OFFp 0), R.RECORD vl) = let
		val (def, _, _) = List.nth(vl, n+1)
	      in T.LOADD(fea(r, n), def)
	      end
	    | chase(r, CPS.SELp(n, CPS.OFFp 0), R.OFFSET(i, vl)) = let
		val (def, _, _) = List.nth(vl, n+i+1)
	      in T.LOADD(fea(r, n), def)
	      end
	    | chase(r, CPS.SELp(n,p), R.RO_MEM) = 
		chase(T.LOAD32(indexEA(r, n), R.RO_MEM), p, R.RO_MEM)
	    | chase(r, CPS.SELp(j,p), R.RECORD vl) = let
		val (def, root, ap) = List.nth(vl, j+1)
  	      in chase(T.LOAD32(indexEA(r,j), def), p, R.trace(root, ap))
	      end
	    | chase(r, CPS.SELp(j,p), R.OFFSET(i, vl)) = let
	        val (def, root, ap) = List.nth(vl, i+j+1)
	      in chase(T.LOAD32(indexEA(r,j), def), p, R.trace(root, ap))
	      end
	in chase(r, path, mem)
	end

    fun fstoreFields ([], _, []) = ()
      | fstoreFields ((v, p)::rest, n, (def, root, _)::regions) = 
	  (emit(T.STORED(T.ADD(C.allocptr, T.LI n), fgetfield(v,p,root), def));
	   fstoreFields(rest, n + 8, regions))
  in
    emit(T.STORE32(ea(C.allocptr, hp), desc, #1 descRegion));
    fstoreFields(fields, hp+4, regions);
    emit(T.MV(ans, T.ADD(C.allocptr, T.LI(hp+4))))
  end	
end

(*
 * $Log$
 *)
