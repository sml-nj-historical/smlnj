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
 * [struct{t1,...tn}]   = unit * [t1] * ... * [tn]
 * [void]               = unit
 *
 * Currently we don't encode arrays.  (C arrays are mostly like pointers
 * except within structures.  For the latter case, we can simulate the
 * desired effect by making n fields of the same type.)
 *
 * The prototype of a function taking arguments of types a1,...,an (n > 0)
 * and producing a result of type r is encoded as:
 *       (unit * [a1] * ... * [an] -> [r]) list
 * We use
 *       (unit * [a1] * ... * [an] -> [r]) list list
 * to specify a "stdcall" calling convention used by, e.g., Win32.
 * 
 * For n = 0 (C argument list is "(void)"), we use:
 *       (unit -> [r]) list     or      (unit -> [r]) list list
 * The use of list constructor(s) here is a trick to avoid having to construct
 * an actual function value of the required type when invoking the RAW_CCALL
 * primop.  Instead, we just pass nil.  The code generator will throw away
 * this value anyway.
 * The unit type for non-empty records and non-empty argument lists
 * avoids the degenerate case of 1-element (ML-)records.
 *)
structure CProto : sig
    exception BadEncoding
    (* Decode the encoding described above.
     * Construct an indicator list for the _actual_ ML arguments of
     * a raw C call and the result type of a raw C call. 
     * Each indicator specifies whether the arguments/result is
     * a float pointer value, an 32-bit integer, or an ML pointer.
     *)
    val decode : string -> 
                 { encoding : Types.ty,
                   arg_ty   : Types.ty,
                   res_ty   : Types.ty
                 }  -> 
                 { c_proto   : CTypes.c_proto,
                   arg_types : PrimOp.ccall_type list,
                   res_type  : PrimOp.ccall_type option
                 }

    (* formatting of C type info (for debugging purposes) *)
    val tshow : CTypes.c_type -> string
    val pshow : CTypes.c_proto -> string
end = struct
    exception BadEncoding

    local
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
        fun decode conv {encoding=t,arg_ty,res_ty} = let
	    (* The type-mapping table: *)
	    val m = [(BT.intTy,           CT.C_signed   CT.I_int),
		     (BT.wordTy,          CT.C_unsigned CT.I_int),
		     (BT.stringTy,        CT.C_PTR),
		     (BT.boolTy,          CT.C_PTR),
		     (BT.realTy,          CT.C_double),
		     (listTy BT.realTy,   CT.C_float),
		     (BT.charTy,          CT.C_signed   CT.I_char),
		     (BT.word8Ty,         CT.C_unsigned CT.I_char),
		     (BT.int32Ty,         CT.C_signed   CT.I_long),
		     (BT.word32Ty,        CT.C_unsigned CT.I_long),
		     (listTy BT.charTy,   CT.C_signed   CT.I_short),
		     (listTy BT.word8Ty,  CT.C_unsigned CT.I_short),
		     (listTy BT.int32Ty,  CT.C_signed   CT.I_long_long),
		     (listTy BT.word32Ty, CT.C_unsigned CT.I_long_long),
		     (listTy (listTy BT.realTy), CT.C_long_double),
		     (BT.exnTy,           CT.C_STRUCT []),
		     (BT.unitTy,          CT.C_void)]

	    fun look t =
		Option.map #2 (List.find (fn (u, _) => TU.equalType (t, u)) m)

	    fun unlist (T.VARty (ref (T.INSTANTIATED t)), i) = unlist (t, i)
	      | unlist (t0 as T.CONty (tc, [t]), i) =
		if TU.equalTycon (tc, BT.listTycon) then unlist (t, i + 1)
		else (t0, i)
	      | unlist (t, i) = (t, i)

            fun isMLTy t = TU.equalType (t, BT.boolTy)

	    (* Given [T] (see above), produce the CTypes.c_type value
	     * corresponding to T. *)
	    fun dt t =
		case look t of
		    SOME ct => (ct, isMLTy t)
		  | NONE =>
		    (case BT.getFields t of
		      SOME (_ :: fl) => (CT.C_STRUCT (map (#1 o dt) fl), false)
		    | _ => bad ()
                    )

	    val (fty, _) = unlist (t, 0)

	    fun getTy(t, true) = PrimOp.CCALL_ML_PTR
              | getTy(t, false) = 
                if TU.equalType (t, BT.realTy) then PrimOp.CCALL_REAL64
                else PrimOp.CCALL_INT32

	    fun res_type(t, ml) =
		if TU.equalType (t, BT.unitTy) then NONE
		else SOME (getTy(t, ml))

            fun getTys(t::tys, ml::mls) = getTy(t,ml) :: getTys(tys,mls)
              | getTys(tys, []) = map (fn t => getTy(t,false)) tys
              | getTys _ = []

	    fun arg_types(t, ml) =
		if TU.equalType (t, BT.unitTy) then [] (* no arg case *)
		else case BT.getFields t of
			 SOME fl => getTys (fl,ml) (* >1 arg case *)
		       | NONE => getTys([t], ml)   (* 1 arg case *)
	in
	    (* Get argument types and result type; decode them.
	     * Construct the corresponding CTypes.c_proto value. *)
	    case getDomainRange fty of
		NONE => bad ()
	      | SOME (d, r) =>
                let val (retTy, retML) = dt r
                    val (argTys, argsML) =
                        if TU.equalType (d, BT.unitTy) then ([], [])
                        else case BT.getFields d of
		               SOME (_ :: fl) =>
                               let val args = map dt fl
                               in  (map #1 args, map #2 args)
                               end
			     | _ => bad () 
                in  {c_proto={ conv = conv,
		               retTy = retTy,
                               paramTys = argTys
                             },
                     arg_types=arg_types(arg_ty, argsML),
                     res_type =res_type(res_ty, retML)
                    }
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
		concat ("{" :: foldr (fn (f, l) => ct f :: ";" :: l) ["}"] fl)
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
