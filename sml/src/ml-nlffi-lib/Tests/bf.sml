structure BF = struct

    structure Anon :> sig type s_1 type s_2 end = struct
        type s_1 = unit type s_2 = unit
    end

    local
	open Tag
    in
        type u_u = u t_u
	type s_1 = Anon.s_1
	type s_2 = Anon.s_2
	type 'c u_u_obj = (u_u, 'c) C.su_obj
	type 'c s_1_obj = (s_1, 'c) C.su_obj
	type 'c s_2_obj = (s_2, 'c) C.su_obj
	val u_u_typ : u_u C.T.su_typ = C_Int.mk_su_typ 0w4
	val s_1_typ : s_1 C.T.su_typ = C_Int.mk_su_typ 0w4
	val s_2_typ : s_2 C.T.su_typ = C_Int.mk_su_typ 0w4
    end

    structure U_u = struct
        local
	    open C_Int
	in
            val typ = u_u_typ
	    val typ_s = s_1_typ
	    val typ_u = s_2_typ
	    fun f_s (x: 'c u_u_obj) = mk_rw_field typ_s 0 x
	    fun f_u (x: 'c u_u_obj) = mk_rw_field typ_u 0 x
	end
    end

    structure S_1 = struct
        local
	    open C_Int
	in
	    val typ = s_1_typ
	    fun bf_x (x: 'c s_1_obj) =
		mk_rw_sbf { offset = 0, bits = 0w3, shift = 0w5 } x
	end
    end
    
    structure S_2 = struct
        local
	    open C_Int
	in
	    val typ = s_2_typ
	    fun bf_x (x: 'c s_2_obj) =
		mk_rw_ubf { offset = 0, bits = 0w3, shift = 0w5 } x
	end
    end

    val SOME f = C.new U_u.typ

    fun s2u x =
	(C.Set.sbf (S_1.bf_x (U_u.f_s f), x);
	 C.Get.ubf (S_2.bf_x (U_u.f_u f)))

    fun u2s x =
	(C.Set.ubf (S_2.bf_x (U_u.f_u f), x);
	 C.Get.sbf (S_1.bf_x (U_u.f_s f)))
end
