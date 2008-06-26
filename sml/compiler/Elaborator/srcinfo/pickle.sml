local
    structure PU = PickleUtil
    structure UU = UnpickleUtil
    structure PSP = PickleSymPid
    structure USP = UnpickleSymPid
    structure EV = Ens_var
in
    structure Ens_pickle = 
    struct
        infix $
        
	type location = string * int * int
	type sign = {sign : Modules.Signature, 
		     def : location, 
		     alias : (location * Symbol.symbol) list ref, 
		     usage : (location * Symbol.symbol) list ref} list

	(* pickling part *)
        val w_int = PU.w_int
	val w_string = PU.w_string
	val w_list = PU.w_list
	val w_pair = PU.w_pair
	val w_symbol = PSP.w_symbol
	val w_option = PU.w_option
			   
	local
	    val T = ~102 (* dangerous too*)
	in
	    fun w_triplet (wa, wb, wc) (a, b, c) =
		let val op $ = PU.$ T in
		    "t" $ [wa a, wb b, wc c]
		end
	end

	local
	    val Q = ~103 (* dangerous too*)
	in
	    fun w_quadruplet (wa, wb, wc, wd) (a, b, c, d) = 
		let val op $ = PU.$ Q in
		    "q" $ [wa a, wb b, wc c, wd d]
		end
	end

	local
	    val Q = ~104 (* dangerous too*)
	in
	    fun w_quintuplet (wa, wb, wc, wd, we) (a, b, c, d, e) =
		let val op $ = PU.$ Q in
		    "q" $ [wa a, wb b, wc c, wd d, we e]
		end
	end

	(*type pickle = sign PU.pickle*)
	val w_vide = fn x => w_list w_int []
	val w_ty = (w_vide:(sign, Types.ty) PU.pickler)
	val w_location = 
	    w_triplet (w_string:(sign, string) PU.pickler, w_int, w_int)
	val w_usage_item = w_pair (w_location, w_ty)
	val w_usage = w_list w_usage_item
	val w_def = w_location
	val w_symbol_list=(w_list w_symbol:(sign, Symbol.symbol list) PU.pickler)
	val w_path = fn (SymPath.SPATH x) => w_symbol_list x
	val w_rpath = fn (InvPath.IPATH x) => w_symbol_list x
	val w_stamp = PSP.w_pid
	val w_access = w_vide
	val w_ty = w_vide
	val w_tycon = w_vide

	local
	    val w_variable = 
	     fn (VarCon.VALvar {path, typ = ref ty, access, ...}) => 
		w_triplet (w_path, w_ty, w_access) (path, ty, access)
	    val w_var_item = 
		fn {var:VarCon.var, def = x, usage = ref y} =>
		   w_triplet (w_variable, w_def, w_usage) (var, x, y)
	in
	    val w_var = w_list w_var_item
	end

	local
	    val w_type_item = 
		fn {tycon:Types.tycon, def = x, usage = ref y} =>
		   w_triplet (w_tycon, w_def, w_usage) (tycon, x, y)
	in
	    val w_type = w_list w_type_item
	end

	local
	    val w_constructor = 
	     fn Types.DATACON {name, typ, ...} => 
		w_pair (w_symbol, w_ty) (name, typ)
	    val w_cons_item = 
	     fn {cons:Types.datacon, def = x, usage = ref y} =>
		w_triplet (w_constructor, w_def, w_usage) (cons, x, y)
	in
	    val w_cons = w_list w_cons_item
	end
			
	local
	    val w_map_item = w_pair (w_int, 
				     w_access : (sign, Access.access) PU.pickler)
	    val w_map = w_list w_map_item
	    val w_structure = w_vide
	    val w_str_item = 
	     fn {str:Modules.Structure, 
		 def = x, 
		 usage = ref y, 
		 map = ref z} =>
		w_quadruplet (w_structure, w_def, w_usage, w_map) 
			     (str, x, y, z)
	in
	   val w_str = w_list w_str_item
	end

	local
	    val w_alias_item = w_pair (w_location, w_symbol)
	    val w_usage_item = w_alias_item
	    val w_alias = w_list w_alias_item
	    val w_usage = w_list w_usage_item
	    val w_signature = w_vide
	     (*fn Modules.SIG {stamp, name, ...} =>
		w_pair (w_stamp, (w_option w_symbol:
				  (sign, Symbol.symbol option) PU.pickler)
		       )
		       (stamp, name)*)
	    val w_sig_item =
	     fn {sign:Modules.Signature, 
		 def = x, 
		 alias = ref y, 
		 usage = ref z} => 
		w_quadruplet (w_signature, w_def, w_alias, w_usage) 
			     (sign, x, y, z)
	in
	    val w_sig = w_list w_sig_item
	end

	val pickle_all = w_quintuplet (w_var, w_type, w_cons, w_str, w_sig)

	fun test () = 
	    let
		val all = EV.give_all () 
		val str_pick = PU.pickle [] (pickle_all all)
	    in 
		str_pick 
	    end

	(* unpickling part *)
	val r_int = UU.r_int
	val r_string = UU.r_string
	val r_pair = UU.r_pair
	val r_list = UU.r_list
	val r_option = UU.r_option
	fun r_triplet session m (r_a, r_b, r_c) () = let
	    fun p #"t" = (r_a (), r_b (), r_c ())
	      | p _ = raise UU.Format
	in
	    UU.share session m p
	end
	fun r_quadruplet session m (r_a, r_b, r_c, r_d) () = let
	    fun p #"q" = (r_a (), r_b (), r_c (), r_d ())
	      | p _ = raise UU.Format
	in
	    UU.share session m p
	end
	fun r_quintuplet session m (r_a, r_b, r_c, r_d, r_e) () = let
	    fun p #"q" = (r_a (), r_b (), r_c (), r_d (), r_e ())
	      | p _ = raise UU.Format
	in
	    UU.share session m p
	end


	type 'v map = 'v UU.map
	val mkMap = UU.mkMap
	fun r_symbol session= USP.r_symbol (session, r_string session)
	fun r_stamp session= USP.r_pid (session, r_string session)
	fun r_symbol_list session = r_list session (mkMap ()) (r_symbol session)
	fun r_path session () = SymPath.SPATH (r_symbol_list session ())
	fun r_rpath session () = InvPath.IPATH (r_symbol_list session ())
	fun r_vide session = r_list session (mkMap ()) (r_int session)
	fun r_location session = 
	    r_triplet session (mkMap ())
		      (r_string session, r_int session, r_int session)
	fun r_def session = r_location session
	fun r_ty session () = 
	    let val _ = r_vide session () in
		Types.UNDEFty
	    end
	fun r_access session () = 
	    let val _ = r_vide session () in
		Access.NO_ACCESS
	    end
	fun r_tycon session () = 
	    let val _ = r_vide session () in
		Types.ERRORtyc
	    end
	fun r_usage_item session = 
	    r_pair session (mkMap ()) (r_location session, r_ty session)
	fun r_usage session = 
	    r_list session (mkMap ()) (r_usage_item session)

	local
	    fun r_variable session () = 
		let val _ = r_path session () 
		    val _ = r_ty session ()
		    val _ = r_access session ()
		in
		    VarCon.ERRORvar
		end
	    fun r_var_item session () = 
		let
		    val (a, b, c) = r_triplet session (mkMap ())
				    (r_variable session,
				     r_def session,
				     r_usage session
				    )
				    ()
		in
		    {var = a, def = b, usage = ref c}
		end
	in
	    fun r_var session = 
		r_list session (mkMap ()) (r_var_item session)
	end

	local
	    fun r_type_item session () = 
		let 
		    val (a, b, c) = r_triplet session (mkMap ())
				    (r_tycon session,
				     r_def session, 
				     r_usage session)
				    ()
		in
		    {tycon = a, def = b, usage = ref c}
		end
	in
	    fun r_type session = 
		r_list session (mkMap ()) (r_type_item session)
	end

	local 
	    fun r_constructor session () = 
		Types.DATACON {name = r_symbol session (),
			       typ = r_ty session (), 
			       rep = Access.UNTAGGED, 
			       lazyp = false, 
			       const = false, 
			       sign = Access.CNIL}
	
	    fun r_cons_item session () = 
		let 
		    val (a, b, c) = 
			r_triplet session (mkMap ()) 
			(r_constructor session,
			 r_def session,
			 r_usage session
			)
			()
		in
		    {cons = a, def = b, usage = ref c}
		end
	in
	    fun r_cons session = 
		r_list session (mkMap ()) (r_cons_item session)
	end

	local
	    fun r_map_item session =
		r_pair session (mkMap ()) (r_int session, r_access session)
	    fun r_map session = 
		r_list session (mkMap ()) (r_map_item session)
	    fun r_structure session () = 
		let val _ = r_vide session () in
		    Modules.ERRORstr
		end
	    fun r_str_item session () = 
		let 
		    val (s, a, b, c) = 
			r_quadruplet session (mkMap ())
				     (r_structure session,
				      r_def session,
				      r_usage session,
				      r_map session)
				     ()
		in
		    {str = s, def = a, usage = ref b, map = ref c}
		end
	in
	    fun r_str session = 
		r_list session (mkMap ()) (r_str_item session)
	end

	local
	    fun r_alias_item session = 
		r_pair session 
		       (mkMap ()) 
		       (r_location session, r_symbol session)
	    fun r_alias session = 
		r_list session (mkMap ()) (r_alias_item session)
	    fun r_usage_item session = r_alias_item session
	    fun r_usage session =
		r_list session (mkMap ()) (r_usage_item session)
	    fun r_signature session () = 
		let val _ = r_stamp session ()
		    val _ = r_option session (mkMap ()) (r_symbol session)
		in
		    Modules.ERRORsig
		end
	    fun r_sig_item session () = 
		let 
		    val (s, a, b, c) = 
			r_quadruplet session (mkMap ()) 
				     (r_signature session, 
				      r_def session, 
				      r_alias session, 
				      r_usage session)
				     ()
		in
		    {sign = s, def = a, alias = ref b, usage = ref c}
		end
	in
	    fun r_sig session = 
		r_list session (mkMap ()) (r_sig_item session)
	end

	fun read_all session = 
	    r_quintuplet session (mkMap ()) (r_var session,
					     r_type session,
					     r_cons session,
					     r_str session,
					     r_sig session
					    )
	fun test_back s = 
	    let
		val session = UU.mkSession (UU.stringGetter s)
		val unpickler = read_all session
	    in
		unpickler ()
	    end
    end
end
