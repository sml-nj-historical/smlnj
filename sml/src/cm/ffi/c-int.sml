(*
 * The implementation of the interface that encodes C's type system
 * in ML.  This implementation includes its "private" extensions.
 *
 *   (C) 2000, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure C_Int :> C_INT = struct

    fun bug m = raise Fail ("impossible: " ^ m)

    type addr = CMemory.addr

    datatype 'f objt =
	BASE of word
      | PTR of 'f objt
      | FPTR of addr -> 'f
      | ARR of { typ: 'f objt, nelem: word, size: word }

    type ('t, 'f, 'c) obj = addr * 'f objt (* RTI for stored value *)
    type ('t, 'f, 'c) obj' = addr

    type ro = unit
    type rw = unit

    type ('t, 'f, 'c) ptr = addr * 'f objt (* RTI for target value *)
    type ('t, 'f, 'c) ptr' = addr

    type ('t, 'f, 'n) arr = unit

    type 'f fptr = addr * 'f
    type 'f fptr' = addr		(* does not carry function around *)

    type voidptr = addr
    type 'tag su = unit

    type schar = CMemory.schar
    type uchar = CMemory.uchar
    type sint = CMemory.sint
    type uint = CMemory.uint
    type sshort = CMemory.sshort
    type ushort = CMemory.ushort
    type slong = CMemory.slong
    type ulong = CMemory.ulong
    type float = CMemory.float
    type double = CMemory.double

    type ml_schar  = CMemory.schar
    type ml_uchar  = CMemory.uchar
    type ml_sint   = CMemory.sint
    type ml_uint   = CMemory.uint
    type ml_sshort = CMemory.sshort
    type ml_ushort = CMemory.ushort
    type ml_slong  = CMemory.slong
    type ml_ulong  = CMemory.ulong
    type ml_float  = CMemory.float
    type ml_double = CMemory.double

    type 'c schar_obj = (schar, unit, 'c) obj
    type 'c uchar_obj = (uchar, unit, 'c) obj
    type 'c sint_obj = (sint, unit, 'c) obj
    type 'c uint_obj = (uint, unit, 'c) obj
    type 'c sshort_obj = (sshort, unit, 'c) obj
    type 'c ushort_obj = (ushort, unit, 'c) obj
    type 'c slong_obj = (slong, unit, 'c) obj
    type 'c ulong_obj = (ulong, unit, 'c) obj
    type 'c float_obj = (float, unit, 'c) obj
    type 'c double_obj = (double, unit, 'c) obj
    type 'c voidptr_obj = (voidptr, unit, 'c) obj
    type ('f, 'c) fptr_obj = ('f fptr, 'f, 'c) obj
    type ('s, 'c) su_obj = ('s su, unit, 'c) obj

    type 'c schar_obj' = (schar, unit, 'c) obj'
    type 'c uchar_obj' = (uchar, unit, 'c) obj'
    type 'c sint_obj' = (sint, unit, 'c) obj'
    type 'c uint_obj' = (uint, unit, 'c) obj'
    type 'c sshort_obj' = (sshort, unit, 'c) obj'
    type 'c ushort_obj' = (ushort, unit, 'c) obj'
    type 'c slong_obj' = (slong, unit, 'c) obj'
    type 'c ulong_obj' = (ulong, unit, 'c) obj'
    type 'c float_obj' = (float, unit, 'c) obj'
    type 'c double_obj' = (double, unit, 'c) obj'
    type 'c voidptr_obj' = (voidptr, unit, 'c) obj'
    type ('f, 'c) fptr_obj' = ('f fptr, 'f, 'c) obj'
    type ('s, 'c) su_obj' = ('s su, unit, 'c) obj'

    type ('s, 'm, 'f) rw_acc = int * 'f objt (* RTI for member object *)
    type ('s, 'm, 'f) ro_acc = int * 'f objt (* RTI for member object *)

    type ('s, 'm, 'f) rw_acc' = int
    type ('s, 'm, 'f) ro_acc' = int

    type 'c bitfield = { wordaddr: addr, mask: ml_uint, shift: word }
    type 's bf_rw_acc = { offset: int, mask: ml_uint, shift: word }
    type 's bf_ro_acc = { offset: int, mask: ml_uint, shift: word }

    type 'n dim = ('n, Dim.nonzero) Dim.dim

    structure T = struct

        type ('t, 'f) typ = 'f objt

	type schar_typ = (schar, unit) typ
	type uchar_typ = (uchar, unit) typ
	type sint_typ = (sint, unit) typ
	type uint_typ = (uint, unit) typ
	type sshort_typ = (sshort, unit) typ
	type ushort_typ = (ushort, unit) typ
	type slong_typ = (slong, unit) typ
	type ulong_typ = (ulong, unit) typ
	type float_typ = (float, unit) typ
	type double_typ = (double, unit) typ
	type voidptr_typ = (voidptr, unit) typ
	type 'f fptr_typ = ('f fptr, 'f) typ
	type 's su_typ = ('s su, unit) typ

	fun typeof (_: addr, t: 'f objt) = t

	fun sizeof (BASE b) = b
	  | sizeof (PTR _) = CMemory.addr_size
	  | sizeof (FPTR _) = CMemory.addr_size
	  | sizeof (ARR a) = #size a

	fun pointer t = PTR t
	fun target (PTR t) = t
	  | target _ = bug "T.target (non-pointer type)"
	fun arr (t, d) = let
	    val n = Word.fromInt (Dim.toInt d)
	in
	    ARR { typ = t, nelem = n, size = n * sizeof t }
	end
	fun elem (ARR a) = #typ a
	  | elem _ = bug "T.elem (non-array type)"
	fun ro (t: 'f objt) = t

	val schar  = BASE CMemory.char_size
	val uchar  = BASE CMemory.char_size
	val sint   = BASE CMemory.int_size
	val uint   = BASE CMemory.int_size
	val sshort = BASE CMemory.short_size
	val ushort = BASE CMemory.short_size
	val slong  = BASE CMemory.long_size
	val ulong  = BASE CMemory.long_size
	val float  = BASE CMemory.float_size
	val double = BASE CMemory.double_size

	val voidptr = BASE CMemory.addr_size
    end

    local
	fun pair_type_addr (t: 'f objt) (a: addr) = (a, t)
	fun pair_type_int (t: 'f objt) (i: int) = (i, t)
	fun acc_strip_type (i: int, _: 'f objt) = i
    in
        val heavy_obj = pair_type_addr
	val heavy_ptr = pair_type_addr

	val heavy_rw_acc = pair_type_int
	val heavy_ro_acc = pair_type_int

	val light_rw_acc = acc_strip_type
	val light_ro_acc = acc_strip_type

	fun heavy_fptr (FPTR mkf) p = (p, mkf p)
	  | heavy_fptr _ _ = bug "heavy_fptr (non-function-pointer-type)"

	(* ------------- internal stuff ------------- *)
	val mk_rw_acc = pair_type_int
	val mk_ro_acc = pair_type_int
    end

    fun sizeof (_: addr, t) = T.sizeof t

    val get_uchar' = CMemory.load_uchar
    val get_schar' = CMemory.char_u2s o CMemory.load_uchar
    val get_uint' = CMemory.load_uint
    val get_sint' = CMemory.int_u2s o CMemory.load_uint
    val get_ushort' = CMemory.load_ushort
    val get_sshort' = CMemory.short_u2s o CMemory.load_ushort
    val get_ulong' = CMemory.load_ulong
    val get_slong' = CMemory.long_u2s o CMemory.load_ulong
    val get_float' = CMemory.load_float
    val get_double' = CMemory.load_double

    fun get_ptr (a, PTR t) = (CMemory.load_addr a, t)
      | get_ptr _ = bug "get_ptr (non-pointer)"
    fun get_fptr (a, FPTR mkf) =
	let val fa = CMemory.load_addr a in (fa, mkf fa) end
      | get_fptr _ = bug "get_fptr (non-function-pointer)"

    val get_ptr' = CMemory.load_addr
    val get_fptr' = CMemory.load_addr
    val get_voidptr' = CMemory.load_addr

    local
	infix $
	fun (f $ g) (x, y) = f (x, g y)
    in
        val set_uchar' = CMemory.store_uchar
	val set_schar' = CMemory.store_uchar $ CMemory.char_s2u
	val set_uint' = CMemory.store_uint
	val set_sint' = CMemory.store_uint $ CMemory.int_s2u
	val set_ushort' = CMemory.store_ushort
	val set_sshort' = CMemory.store_ushort $ CMemory.short_s2u
	val set_ulong' = CMemory.store_ulong
	val set_slong' = CMemory.store_ulong $ CMemory.long_s2u
	val set_float' = CMemory.store_float
	val set_double' = CMemory.store_double
    end

    val set_ptr' = CMemory.store_addr
    val set_fptr' = CMemory.store_addr
    val set_voidptr' = CMemory.store_addr
    val set_ptr_voidptr' = CMemory.store_addr

    fun copy' t =
	let val bytes = T.sizeof t
	in fn { from, to } =>
	      CMemory.bcopy { from = from, to = to, bytes = bytes }
	end
    fun copy { from = (from, t), to = (to, _: 'f objt) } =
	copy' t { from = from, to = to }

    local
	fun addr_type_id (x: addr * 'f objt) = x
    in
        val |&| = addr_type_id
	val |*| = addr_type_id
	val ro = addr_type_id
	val rw = addr_type_id
    end

    local
	fun addr_id (x: addr) = x
    in
        val |&&| = addr_id
	val |**| = addr_id
	val ro' = addr_id
	val rw' = addr_id
	val ptr_inject' = addr_id
	fun ptr_project' (_ : 'f objt) = addr_id
	val arr_decay' = addr_id
    end

    local
	fun strip_type (a: addr, _: unit objt) = a
	fun p_strip_type (a: addr, _: 'f objt) = a
	fun strip_fun (a: addr, _: 'f) = a
	infix $
	fun (f $ g) (x, y) = f (g x, y)
    in
        val ptr_inject = p_strip_type
	val light_obj = p_strip_type
	val light_ptr = p_strip_type
	val light_fptr = strip_fun

	val get_uchar = get_uchar' o strip_type
	val get_schar = get_schar' o strip_type
	val get_uint = get_uint' o strip_type
	val get_sint = get_sint' o strip_type
	val get_ushort = get_ushort' o strip_type
	val get_sshort = get_sshort' o strip_type
	val get_ulong = get_ulong' o strip_type
	val get_slong = get_slong' o strip_type
	val get_float = get_float' o strip_type
	val get_double = get_double' o strip_type
	val get_voidptr = get_voidptr' o strip_type

	val set_uchar = set_uchar' $ strip_type
	val set_schar = set_schar' $ strip_type
	val set_uint = set_uint' $ strip_type
	val set_sint = set_sint' $ strip_type
	val set_ushort = set_ushort' $ strip_type
	val set_sshort = set_sshort' $ strip_type
	val set_ulong = set_ulong' $ strip_type
	val set_slong = set_slong' $ strip_type
	val set_float = set_float' $ strip_type
	val set_double = set_double' $ strip_type
	val set_voidptr = set_voidptr' $ strip_type

	fun set_ptr_voidptr (x, p) = set_ptr_voidptr' (p_strip_type x, p)

	fun set_ptr (x, p) = set_ptr' (p_strip_type x, p_strip_type p)
	fun set_fptr (x, f) = set_fptr' (p_strip_type x, strip_fun f)

	fun ptr_compare (p, p') =
	    CMemory.compare (p_strip_type p, p_strip_type p')

	val discard' = CMemory.free
	fun discard x = discard' (p_strip_type x)
    end

    val ptr_compare' = CMemory.compare

    fun ptr_project (PTR t) (p : voidptr) = (p, t)
      | ptr_project _ _ = bug "ptr_project (non-pointer-type)"

    local
	fun asub (a, i, ARR { typ, nelem, ... }) =
	    (* take advantage of wrap-around to avoid the >= 0 test... *)
	    if Word.fromInt i < nelem then
		(CMemory.++ (a, Word.toInt (T.sizeof typ) * i), typ)
	    else raise General.Subscript
	  | asub _ = bug "arr_sub(') (non-array)"
    in
        fun arr_sub ((a, t), i) = asub (a, i, t)
	fun arr_sub' t (a, i) = #1 (asub (a, i, t))
    end

    fun arr_decay (a, ARR { typ, ... }) = (a, typ)
      | arr_decay _ = bug "arr_decay (non-array)"

    fun arr_reconstruct ((a: addr, t), d) = (a, T.arr (t, d))

    fun arr_reconstruct' (a: addr, d: 'n dim) = a

    val null = CMemory.null
    val voidptr_null = CMemory.isNull
    fun ptr_null p = voidptr_null (ptr_inject p)

    val ptr_null' = CMemory.isNull

    fun |++| t (p, i) = CMemory.++ (p, Word.toInt (T.sizeof t) * i)
    fun |--| t (p, p') = (CMemory.-- (p, p')) div (Word.toInt (T.sizeof t))

    fun |+| ((p, t), i) = (|++| t (p, i), t)
    fun |-| ((p, t), (p', _: 'f objt)) = |--| t (p, p')

    fun ptr_sub (p, i) = |*| (|+| (p, i))

    fun ptr_sub' t (p, i) = |**| (|++| t (p, i))

    fun |$| ((a, _: unit objt), (i, t: 'f objt)) = (CMemory.++ (a, i), t)
    val |$! = |$|
    fun |#| ((a, _: unit objt), { offset, mask, shift }: 's bf_rw_acc) =
	{ wordaddr = CMemory.++ (a, offset), mask = mask, shift = shift }
    val |#! = |#|

    val |$$| = CMemory.++
    val |$$! = |$$|
    fun |##| (a, { offset, mask, shift }: 's bf_rw_acc) =
	{ wordaddr = CMemory.++ (a, offset), mask = mask, shift = shift }
    val |##! = |##|

    fun get_bf { wordaddr, mask, shift } =
	CMemory.>> (CMemory.andb (CMemory.load_uint wordaddr, mask), shift)

    fun set_bf ({ wordaddr, mask, shift }, x) =
	CMemory.store_uint
	    (wordaddr,
	     CMemory.orb (CMemory.andb (CMemory.load_uint wordaddr,
					CMemory.notb mask),
			  CMemory.andb (CMemory.<< (x, shift), mask)))

    fun new' t = CMemory.alloc (T.sizeof t)
    fun new t = Option.map (fn a => (a, t)) (new' t)

    fun call ((_: addr, f), x) = f x

    fun call' (FPTR mkf) (a, x) = mkf a x
      | call' _ _ = bug "call' (non-function-pointer-type)"

    (* ------------- internal stuff ------------- *)

    fun mk_ptr (PTR t) (a : addr) = (a, t)
      | mk_ptr _ _ = bug "mk_ptr (non-pointer-type)"
    fun mk_voidptr (a : addr) = a
    fun mk_fptr (FPTR mkf) a = (a, mkf a)
      | mk_fptr _ _ = bug "mk_fptr (non-function-pointer-type)"

    fun mk_bf_rw_acc a = a : 's bf_rw_acc
    fun mk_bf_ro_acc a = a : 's bf_ro_acc

    fun mk_su_typ sz = BASE sz
    fun mk_fptr_typ (mkf: addr -> 'a -> 'b) = FPTR mkf
end
