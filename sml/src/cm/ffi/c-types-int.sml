(*
 * The implementation of the interface that encodes C's type system
 * in ML.  This implementation includes its "private" extensions.
 *
 *   (C) 2000, Lucent Technologies, Bell Laboratories
 *
 * author: Matthias Blume (blume@kurims.kyoto-u.ac.jp)
 *)
structure CTypesInternal :> CTYPES_INTERNAL = struct

    type addr = CMemory.addr

    (* C objects and their types *)

    (* "objtype" remembers how many levels of pointer indirection we have.
     * Eventually, it knows the size of the base type. *)
    datatype objt =
	     T_BASE of word		(* size of base type *)
	   | T_PTR of objt		(* pointer *)
	   | T_ARR of objt * word	(* array *)
	
    type 't objtype = objt

    fun sizeof (T_BASE sz) = sz
      | sizeof (T_PTR _) = CMemory.addr_size
      | sizeof (T_ARR (t, l)) = l * sizeof t

    (* An object is known by its address. *)
    type 't obj = addr

    (* Things that can be substituted for 't... *)

    (* these are dummies, but we make them the same as the ml_versions *)
    type c_char = CMemory.schar
    type c_uchar = CMemory.uchar
    type c_int = CMemory.sint
    type c_uint = CMemory.uint
    type c_short = CMemory.sshort
    type c_ushort = CMemory.ushort
    type c_long = CMemory.slong
    type c_ulong = CMemory.ulong
    type c_float = CMemory.float
    type c_double = CMemory.double

    (* pointers are just addresses *)
    type 't ptr = addr

    (* "void *", this is also just an address, but it is NOT a pointer! *)
    type c_voidptr = addr

    (* function pointers *)
    type 's fptr = addr

    (* the array type is a dummy placeholder, there are no array values *)
    type ('t, 'n) arr = unit

    (* same for structs and unions with tag 'tag *)
    type 'tag su = unit

    (* The same for the ML side... *)
    type ml_char = CMemory.schar
    type ml_uchar = CMemory.uchar
    type ml_int = CMemory.sint
    type ml_uint = CMemory.uint
    type ml_short = CMemory.sshort
    type ml_ushort = CMemory.ushort
    type ml_long = CMemory.slong
    type ml_ulong = CMemory.ulong
    type ml_float = CMemory.float
    type ml_double = CMemory.double

    val t_char : c_char objtype = T_BASE CMemory.char_size
    val t_uchar : c_uchar objtype = T_BASE CMemory.char_size
    val t_int : c_int objtype = T_BASE CMemory.int_size
    val t_uint : c_uint objtype = T_BASE CMemory.int_size
    val t_short : c_short objtype = T_BASE CMemory.short_size
    val t_ushort : c_ushort objtype = T_BASE CMemory.short_size
    val t_long : c_long objtype = T_BASE CMemory.long_size
    val t_ulong : c_ulong objtype = T_BASE CMemory.long_size
    val t_float : c_float objtype = T_BASE CMemory.float_size
    val t_double : c_double objtype = T_BASE CMemory.double_size

    (* void* is treated as a base type here *)
    val t_voidptr : c_voidptr objtype = T_BASE CMemory.addr_size

    (* getters for objects having transparent types *)
    fun get_uchar obj = CMemory.load_uchar obj
    fun get_char obj = CMemory.char_u2s (CMemory.load_uchar obj)
    fun get_uint obj = CMemory.load_uint obj
    fun get_int obj = CMemory.int_u2s (CMemory.load_uint obj)
    fun get_ushort obj = CMemory.load_ushort obj
    fun get_short obj = CMemory.short_u2s (CMemory.load_ushort obj)
    fun get_ulong obj = CMemory.load_ulong obj
    fun get_long obj = CMemory.long_u2s (CMemory.load_ulong obj)
    fun get_float obj = CMemory.load_float obj
    fun get_double obj = CMemory.load_double obj

    fun get_ptr obj = CMemory.load_addr obj
    fun get_fptr obj = CMemory.load_addr obj
    fun get_voidptr obj = CMemory.load_addr obj

    (* setters for objects having transparent types *)
    fun set_uchar (a, c) = CMemory.store_uchar (a, c)
    fun set_char (a, c) = CMemory.store_uchar (a, CMemory.char_s2u c)
    fun set_uint (a, i) = CMemory.store_uint (a, i)
    fun set_int (a, i) = CMemory.store_uint (a, CMemory.int_s2u i)
    fun set_ushort (a, s) = CMemory.store_ushort (a, s)
    fun set_short (a, s) = CMemory.store_ushort (a, CMemory.short_s2u s)
    fun set_ulong (a, l) = CMemory.store_ulong (a, l)
    fun set_long (a, l) = CMemory.store_ulong (a, CMemory.long_s2u l)
    fun set_float (a, f) = CMemory.store_float (a, f)
    fun set_double (a, d) = CMemory.store_double (a, d)

    fun set_ptr (a, p) = CMemory.store_addr (a, p)
    fun set_fptr (a, f) = CMemory.store_addr (a, f)
    fun set_voidptr (a, v) = CMemory.store_addr (a, v)

    fun ptr obj = obj
    fun ptr_type et = T_PTR et
    fun target ptr = ptr
    (* Failure handling is here pro-forma only.  The typing magic will
     * prevent target_type to be called on anything but pointer types. *)
    fun target_type (T_PTR et) = et
      | target_type _ = raise Fail "impossible: target_type found non-pointer"
    val null = CMemory.null
    val isNull = CMemory.isNull
    fun padd t (ptr, n) = CMemory.++ (ptr, Word.toInt (sizeof t) * n)
    fun ptr_sub t = target o padd t

    (* Injection into void* is fine; but projection out of it is
     * just horribly unsafe.  Should this be here?!?
     * (I guess so -- that's C programming for you... :) *)
    fun ptr_inject (ptr: 't ptr) = ptr: c_voidptr
    fun ptr_project (_: 't objtype) (ptr: c_voidptr) = ptr : 't ptr

    (* array stuff *)
    fun arr_type (et, d) = T_ARR (et, Word.fromInt (Dim.toInt d))
    (* Again, failure-handling is here pro-forma only.  Cannot happen. *)
    fun elem_type (T_ARR (et, _)) = et
      | elem_type _ = raise Fail "impossible: elem_type found non-array"

    (* array subscript is a lot like pointer arithmetic
     * (who would have thunk... :) *)
    fun arr_sub (et: 't objtype) (a: ('t, 'n) arr obj, i) =
	CMemory.++ (a, Word.toInt (sizeof et) * i) : 't obj

    (* safe array subscript;
     *  this requires the array type, not just the element type *)
    fun arr_safe_sub (T_ARR (et, len) : ('t, 'n) arr objtype)
		     (a : ('t, 'n) arr obj, i) =
	if i >= 0 andalso Word.fromInt i < len then
	    CMemory.++ (a, Word.toInt (sizeof et) * i) : 't obj
	else raise General.Subscript
      | arr_safe_sub _ _ =
	raise Fail "impossible: arr_safe_sub found non-array"

    (* array decay is trivial as the underlying address stays unchanged *)
    fun arr_decay (a: ('t, 'n) arr obj) = a : 't ptr

    (* "bitfield" encapsulates the struct's address and the access
     * coordinates of the bitfield. *)
    type bitfield = { wordaddr: addr, mask: ml_uint, shift: word }

    (* Fetching involves getting the whole word and doing some bit
     * twiddling on that... *)
    fun getBitfield { wordaddr, mask, shift } =
	CMemory.>> (CMemory.andb (CMemory.load_uint wordaddr, mask), shift)

    (* Setting is more complicated as we have to make sure that the rest
     * of the word is not changed. Handling special cases such as
     * when the bitfield fits into a short or a char aren't worth
     * handling (IMO) because at the hardware level this will
     * nowadays incur even more bitfiddling. *)
    fun setBitfield ({ wordaddr, mask, shift }, x) =
	CMemory.store_uint
	    (wordaddr,
	     CMemory.orb (CMemory.andb (CMemory.load_uint wordaddr,
					CMemory.notb mask),
			  CMemory.andb (CMemory.<< (x, shift), mask)))

    (* ----------------------------------------- *)
    (* Private, (even more :-) unsafe stuff. *)

    fun mkObj (_: 't objtype) (a: addr) = a: 't obj

    (* Generate access functions for struct and union members. *)
    fun mkStructMember
	    { stype = _ : 's su objtype, mtype = _ : 'm objtype,
	      offset = off : int }
	    (obj: 's su obj) =
	    CMemory.++ (obj, off) : 'm obj
    fun mkUnionMember
	    { utype = _ : 's su objtype, mtype = _ : 'm objtype }
	    (obj: 's su obj) =
	    obj : 'm obj
    fun mkBitfield
	    { stype = _ : 's su objtype, offset = off : int,
	      mask = mask : ml_uint, shift = shift : word }
	    (obj: 's su obj) =
	    { wordaddr = CMemory.++ (obj, off),
	      mask = mask, shift = shift } : bitfield

    (* make a struct or union type from its size *)
    fun mkSUType (_: 'tag) (sz: word) = T_BASE sz : 'tag su objtype

    (* generate new function pointer objtype *)
    fun mkFptrType (_: 's) = T_BASE CMemory.addr_size : 's fptr objtype

    (* reveal address representing a given function pointer *)
    fun revealFptr (fp: 's fptr) = fp : addr

    (*  make a function pointer from a given address *)
    fun mkFptr (_: 's fptr objtype) (a: addr) = a : 's fptr
end
