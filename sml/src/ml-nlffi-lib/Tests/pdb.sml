structure PDB = struct

    local
	open Tag
    in
        (* types representing (struct entry) *)
        type s_entry = s t_e t_n t_t t_r t_y            (* struct tag *)
	type 'c s_entry_obj = (s_entry, 'c) C.su_obj    (* object type *)
	type 'c s_entry_obj' = (s_entry, 'c) C.su_obj'  (* light version *)
    end

    (* fields and RTI for (struct entry) *)
    structure S_entry = struct
        local
	    open C_Int
	in
	    (* RTI for (struct entry) itself *)
	    val typ : s_entry T.su_typ = mk_su_typ 0w20

	    (* RTI for fields *)
	    val typ_last = T.pointer T.uchar
	    val typ_first = T.pointer T.uchar
	    val typ_age = T.sshort
	    val typ_weight = T.float
	    val typ_next = T.pointer typ

	    (* field accessors *)
	    fun f_last (x: 'c s_entry_obj) = mk_rw_field typ_last 0 x
	    fun f_first (x: 'c s_entry_obj) = mk_rw_field typ_first 4 x
	    fun f_age (x: 'c s_entry_obj) = mk_rw_field typ_age 8 x
	    fun f_weight (x: 'c s_entry_obj) = mk_rw_field typ_weight 12 x
	    fun f_next (x: 'c s_entry_obj) = mk_rw_field typ_next 16 x

	    (* light versions *)
	    fun f_last' (x: 'c s_entry_obj') : ((uchar, unit, rw) ptr, unit, 'c) obj' = mk_field' 0 x
	    fun f_first' (x: 'c s_entry_obj') : ((uchar, unit, rw) ptr, unit, 'c) obj' = mk_field' 4 x
	    fun f_age' (x: 'c s_entry_obj') : 'c sshort_obj' = mk_field' 8 x
	    fun f_weight' (x: 'c s_entry_obj') : 'c float_obj' = mk_field' 12 x
	    fun f_next' (x: 'c s_entry_obj') : ((s_entry su, unit, rw) ptr, unit, 'c) obj' = mk_field' 16 x
	end
    end

    local
	structure D = DynLinkage
	(* handle on shared module *)
	val so_h = D.open_lib { name = "./pdb.so", lazy = true, global = true }

	(* handle on address of db *)
	val dbaddr_h = D.lib_symbol so_h "db"
	(* handle on address of print *)
	val printaddr_h = D.lib_symbol so_h "print"

	(* low-level call operation for pdb's print-like functions *)
	val print_callop = RawMemInlineT.rawccall
	    : Word32.word		(* function address *)
	      * Word32.word		(* ML argument *)
	      * (unit * string -> int) list (* encoded C prototype *)
	      -> Int32.int		(* ML result *)

	(* high-level call operation (already correctly typed now) *)
	fun print_mkcall a (x: (s_entry C.su, unit, C.rw) C.ptr) =
	    print_callop (a, C_Int.reveal (C.Ptr.inject x), [])
    in
        (* global variable db: a C object of type (struct entry * ) *)
        fun g_db () : ((s_entry C.su, unit, C.rw) C.ptr, unit, C.rw) C.obj =
	    C_Int.mk_obj (C.T.pointer S_entry.typ) (D.addr dbaddr_h)

	(* global function print: RTTI, C function pointer, ML function *)
        val fn_print_typ = C_Int.mk_fptr_typ print_mkcall
	fun fn_print_fptr () = C_Int.mk_fptr fn_print_typ (D.addr printaddr_h)
	fun fn_print x = C.call (fn_print_fptr (), x)
    end
end
