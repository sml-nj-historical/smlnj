(*
 * COPYRIGHT (c) 2001 Bell Labs, Lucent Technologies
 *
 * An ad-hoc encoding of CTypes.c_proto (from MLRISC) in ML types.
 * (This encoding has _nothing_ to do with actual representation types,
 * it is used just for communicating the function call protocol to
 * the backend. All actual ML arguments are passed as Int32.int,
 * Word32.word, and real.)
 *
 * author: Matthias Blume
 *)

(*
 * The following mapping applies:
 *   Given C-type t, we write [t] to denote its encoding in ML types.
 *
 * [double]             = real
 * [float]              = real list
 * [long double]        = real list list
 * [char]               = char
 * [unsigned char]      = Word8.word
 * [int]                = Int31.int
 * [unsigned int]       = Word31.word
 * [long]               = Int32.int
 * [unsigned long]      = Word32.word
 * [short]              = char list
 * [unsigned short]     = Word8.word list
 * [long long]          = Int32.int list
 * [unsigned long long] = Word32.word list
 * [T*]                 = string
 * ml object            = bool
 * [struct {}]          = exn
 * [struct{t1,...,tn}]  = unit * [t1] * ... * [tn]
 * [union{t1,...,tn}]   = int * [t1] * ... * [tn]
 * [void]               = unit
 *
 * Currently we don't encode arrays.  (C arrays are mostly like pointers
 * except within structures.  For the latter case, we can simulate the
 * desired effect by making n fields of the same type.)
 *
 * The prototype of a function taking arguments of types a1,...,an (n > 0)
 * and producing a result of type r is encoded as:
 *       ([conv] * [a1] * ... * [an] -> [r]) list
 *
 * We use
 *       ([conv] * [a1] * ... * [an] -> [r]) list list
 * to specify a reentrant call.
 *
 * For n = 0 (C argument list is "(void)"), we use:
 *       ([conv] -> [r]) list     or      ([conv] -> [r]) list list
 * The use of list constructor(s) here is a trick to avoid having to construct
 * an actual function value of the required type when invoking the RAW_CCALL
 * primop.  Instead, we just pass nil.  The code generator will throw away
 * this value anyway.
 * 
 * The [conv] type for non-empty records and non-empty argument lists
 * has the additional effect of avoiding the degenerate case of
 * 1-element (ML-)records.
 *
 * [conv] encodes the calling convention to be used:
 *     [default]       = unit
 *     [ccall]         = word    -- for x86/win32
 *     [stdcall]       = int     -- for x86/win32
 *)
structure CProto : sig
    exception BadEncoding
    (* Decode the encoding described above.
     * Construct an indicator list for the _actual_ ML arguments of
     * a raw C call and the result type of a raw C call. 
     * Each indicator specifies whether the arguments/result is
     * passed as a 32-bit integer, a 64-bit integer (currently unused),
     * a 64-bit floating point value, or an Unsafe.Object.object.
     *)
    val decode : string ->
		 { fun_ty : Types.ty, encoding : Types.ty } -> 
                 { c_proto    : CTypes.c_proto,
                   ml_args    : PrimOp.ccall_type list,
                   ml_res_opt : PrimOp.ccall_type option,
		   reentrant  : bool }

    (* formatting of C type info (for debugging purposes) *)
    val tshow : CTypes.c_type -> string
    val pshow : CTypes.c_proto -> string
end = struct
    exception BadEncoding

    local
	structure P = PrimOp
	structure T = Types
	structure BT = BasicTypes
	structure CT = CTypes
	structure TU = TypesUtil
	fun getDomainRange t = let
	    fun get (T.VARty (ref (T.INSTANTIATED t))) = get t
	      | get (T.CONty (_, [t, r])) = SOME (t, r)
	      | get _ = NONE
	in
	    if BT.isArrowType t then get t else NONE
	end
	fun bad () = raise BadEncoding
	fun listTy t = T.CONty (BT.listTycon, [t])
    in
        fun decode defaultconv { encoding = t, fun_ty } = let
	    (* The type-mapping table: *)
	    val m =
		[(BT.intTy,           CT.C_signed   CT.I_int,       P.CCI32),
		 (BT.wordTy,          CT.C_unsigned CT.I_int,       P.CCI32),
		 (BT.stringTy,        CT.C_PTR,                     P.CCI32),
		 (BT.boolTy,          CT.C_PTR,                     P.CCML),
		 (BT.realTy,          CT.C_double,                  P.CCR64),
		 (listTy BT.realTy,   CT.C_float,                   P.CCR64),
		 (BT.charTy,          CT.C_signed   CT.I_char,      P.CCI32),
		 (BT.word8Ty,         CT.C_unsigned CT.I_char,      P.CCI32),
		 (BT.int32Ty,         CT.C_signed   CT.I_long,      P.CCI32),
		 (BT.word32Ty,        CT.C_unsigned CT.I_long,      P.CCI32),
		 (listTy BT.charTy,   CT.C_signed   CT.I_short,     P.CCI32),
		 (listTy BT.word8Ty,  CT.C_unsigned CT.I_short,     P.CCI32),
		 (listTy BT.int32Ty,  CT.C_signed   CT.I_long_long, P.CCI32),
		 (listTy BT.word32Ty, CT.C_unsigned CT.I_long_long, P.CCI32),
		 (listTy (listTy BT.realTy), CT.C_long_double,      P.CCR64),
		 (BT.exnTy,           CT.C_STRUCT [],               P.CCI32)]

	    fun look t =
		Option.map (fn (_, x, y) => (x, y))
			   (List.find (fn (u, _, _) => TU.equalType (t, u)) m)

	    fun unlist (T.VARty (ref (T.INSTANTIATED t)), i) = unlist (t, i)
	      | unlist (t0 as T.CONty (tc, [t]), i) =
		if TU.equalTycon (tc, BT.listTycon) then unlist (t, i + 1)
		else (t0, i)
	      | unlist (t, i) = (t, i)

	    (* Given [T] (see above), produce the CTypes.c_type value
	     * and PrimOp.ccall_type corresponding to T: *)
	    fun dt t =
		case look t of
		    SOME tt => tt
		  | NONE =>
		    (case BT.getFields t of
			 SOME (f1 :: fl) =>
			   if TU.equalType (f1, BT.unitTy) then
			       (CT.C_STRUCT (map (#1 o dt) fl), P.CCI32)
			   else
			       (CT.C_UNION (map (#1 o dt) fl), P.CCI32)
		       | _ => bad ())

	    fun rdt (t, ml_args) =
		if TU.equalType (t, BT.unitTy) then
		    (CT.C_void, NONE, ml_args)
		else let val (ct, mt) = dt t
		     in
			 case ct of
			     (CT.C_STRUCT _ | CT.C_UNION _) =>
			       (ct, SOME mt, mt :: ml_args)
			   | _ => (ct, SOME mt, ml_args)
		     end

	    val (fty, nlists) = unlist (t, 0)

	    val reentrant = nlists > 1

	    fun getConv t =
		if TU.equalType (t, BT.unitTy) then SOME defaultconv
		else if TU.equalType (t, BT.wordTy) then SOME "ccall"
		else if TU.equalType (t, BT.intTy) then SOME "stdcall"
		else NONE
	in
	    (* Get argument types and result type; decode them.
	     * Construct the corresponding CTypes.c_proto value. *)
	    case getDomainRange fty of
		NONE => bad ()
	      | SOME (d, r) =>
                let val (conv, argTys, argsML) =
			(case getConv d of
			     SOME conv => (conv, [], [])
			   | NONE =>
			     (case BT.getFields d of
				  SOME (convty :: fl) =>
				    (case getConv convty of
					 SOME conv =>
					 let val (argTys, argsML) =
						 ListPair.unzip (map dt fl)
					 in
					     (conv, argTys, argsML)
					 end
				       | NONE => bad ())
				| _ => bad ()))
		    val (retTy, retML, argsML) = rdt (r, argsML)
                in
		    { c_proto = { conv = conv,
				  retTy = retTy,
				  paramTys = argTys },
                      ml_args = argsML,
                      ml_res_opt = retML,
		      reentrant = reentrant }
                end
	end

	local
	    open CTypes
	    fun ci I_char = "char"
	      | ci I_short = "short"
	      | ci I_int = "int"
	      | ci I_long = "long"
	      | ci I_long_long = "long long"
	    fun ct C_void = "void"
	      | ct C_float = "float"
	      | ct C_double = "double"
	      | ct C_long_double = "long double"
	      | ct (C_unsigned i) = "unsigned " ^ ci i
	      | ct (C_signed i) = ci i
	      | ct C_PTR = "T*"
	      | ct (C_ARRAY(t,i)) = concat [ct t, "[", Int.toString i, "]"]
	      | ct (C_STRUCT fl) =
  		concat ("s{" :: foldr (fn (f, l) => ct f :: ";" :: l) ["}"] fl)
	      | ct (C_UNION fl) =
  		concat ("u{" :: foldr (fn (f, l) => ct f :: ";" :: l) ["}"] fl)
	    fun cp { conv, retTy, paramTys = a1 :: an } =
		concat (ct retTy :: "(*)(" :: ct a1 ::
			foldr (fn (a, l) => "," :: ct a :: l) [")"] an)
	      | cp { conv, retTy, paramTys = [] } = ct retTy ^ "(*)(void)"
	in
	    val tshow = ct
	    val pshow = cp
	end
    end
end
