(* bug1544.1.sml *)

(*****************************************************************************

 *****************************************************************************)

structure Util = struct
fun listCompare eltCompare ([], []) = EQUAL
  | listCompare eltCompare ([], _) = GREATER
  | listCompare eltCompare (_, []) = LESS
  | listCompare eltCompare (x::xs, y::ys) = 
    let val or = eltCompare(x,y)
    in
	if (or = EQUAL) then 
	    listCompare(eltCompare)(xs,ys)
	else 
	    or
    end
end
(*****************************************************************************

 *****************************************************************************)

structure StandardType :
 sig
 
    datatype TypeConstant = 
	 IntType | RealType | CharType | StringType | UnitType

    datatype STLocalPropDatatype = 
	STsources of IntBinarySet.set | 
	STsinks of IntBinarySet.set

    type STLocalPropType = STLocalPropDatatype list ref

     datatype Typ = 
	 VarType of {tv: int, uid: int, 
		     sProp: STLocalPropType}
       | ConstType of {const: TypeConstant, uid: int, 
		       sProp: STLocalPropType}
       | ArrowType of {from: Typ, to: Typ, uid: int, 
		       sProp: STLocalPropType}
       | RecordType of {fields: (string list), 
			types: (Typ list), uid: int,
			sProp: STLocalPropType}
       | IsectType of {fields: (string list), 
		       types: (Typ list), uid: int, sProp: STLocalPropType}
       | VariantType of {tag: string, 
			 tags: (string list), 
			 types: (Typ list), uid: int, 
			 sProp: STLocalPropType}
       | UnionType of {tag: string, 
		       tags: (string list), 
		       types: (Typ list), uid: int, 
		       sProp: STLocalPropType} 
       | RefType of {typ: Typ, uid: int, sProp: STLocalPropType}
       | ArrayType of {typ: Typ, dims: int, uid: int, 
		       sProp: STLocalPropType}
       | ExceptionConstructor of {typ: Typ, uid: int, 
				  sProp: STLocalPropType}
       | Exception of {uid: int, sProp: STLocalPropType}
       | RecType of {btvs: int list, 
		     decls: Typ list, body: Typ, uid: int, 
		     sProp: STLocalPropType}
       | AbsType of {tv: int, body: Typ, uid: int, 
		     sProp: STLocalPropType}
       | AppType of {rator: Typ, rand: Typ, uid: int, 
		     sProp: STLocalPropType}

     val uidOf : Typ -> int
 end
=
 struct
    datatype TypeConstant = 
        IntType 
      | RealType 
      | CharType 
      | StringType 
      | UnitType			(* Abbreviation for empty record *)

    datatype STLocalPropDatatype = 
	STsources of IntBinarySet.set | 
	STsinks of IntBinarySet.set

    type STLocalPropType = STLocalPropDatatype list ref 

    datatype Typ =
	VarType of {tv: int, uid: int, sProp: STLocalPropType}
      | ConstType of {const: TypeConstant, uid: int, sProp: STLocalPropType}
      | ArrowType of {from: Typ, to: Typ, uid: int, sProp: STLocalPropType}
      | RecordType of {fields: (string list), types: (Typ list), uid: int, 
		       sProp: STLocalPropType}
      | IsectType of {fields: (string list), types: (Typ list), uid: int, 
		      sProp: STLocalPropType}
      | VariantType of {tag: string, tags: (string list), 
			types: (Typ list), uid: int, sProp: STLocalPropType}
      | UnionType of {tag: string, tags: (string list), 
		      types: (Typ list), uid: int, sProp: STLocalPropType}
      (*
       * Types for exceptions.  These are for simple type checking of
       * ML's exceptions and are ML specific.
       *)
      | ExceptionConstructor of {typ: Typ, uid: int, sProp: STLocalPropType}
      | Exception of {uid: int, sProp: STLocalPropType}
      (* 
       * One approach to handling mutually recursive types:
       * datatype ... foo = ...
       * and ... bar = ...
       *
       * ... x : foo
       *
       * gives x a type somewhat like:
       * Rectype{btvs={foo,bar},decls=[defn of foo, defn of bar],body=foo}
       *
       *)
      | RecType of {btvs: int list, decls: Typ list, body: Typ, uid: int, 
		    sProp: STLocalPropType}
      (* How to handle parameterized types and type applications? 
       * Here's one approach: 
       *)
      (* AbsType is for defining parameterized types like "'a list" *)
      | AbsType of {tv: int, body: Typ, uid: int, sProp: STLocalPropType}
      (* App Type is for instantiating parameterized types, as in "int list" *)
      | AppType of {rator: Typ, rand: Typ, uid: int, sProp: STLocalPropType}
      | RefType of {typ: Typ, uid: int, sProp: STLocalPropType}
      | ArrayType of {typ: Typ, dims: int, uid: int, sProp: STLocalPropType}

    fun constConst (ConstType{const,...}) = const
      | constConst (_) = raise Fail "StandardType.constConst"


    fun uidOf (VarType{uid, ...}) = uid
      | uidOf (ConstType{uid, ...}) = uid
      | uidOf (ArrowType{uid, ...}) = uid
      | uidOf (RecordType{uid, ...}) = uid
      | uidOf (IsectType{uid, ...}) = uid
      | uidOf (VariantType{uid, ...}) = uid
      | uidOf (UnionType{uid, ...}) = uid
      | uidOf (ExceptionConstructor{uid, ...}) = uid
      | uidOf (Exception{uid, ...}) = uid
      | uidOf (ArrayType{uid, ...}) = uid
      | uidOf (RecType{uid, ...}) = uid
      | uidOf (AbsType{uid, ...}) = uid
      | uidOf (AppType{uid, ...}) = uid
      | uidOf (RefType{uid, ...}) = uid

 end					(* struct *)

structure STcompare :
sig
    (* Comparison for sets and maps of StandardTyp.Typ 
     *
     * The structure STkey : ORD_KEY (at the bottom of this file) is
     * the same structure with a restricted signature.
     *)
    

    (* true if otherwise EQUAL types should be distinguished flow sets *)
    val checkFlowLabels : bool ref

    (* true if a free type variable matches any type
     * N.B. (o) if checkFlowLabels is also set, free type variables 
     *          will still match any type regardless of flow labels.
     *      (o) don't set freeVarMatchesAll and expect STset / STmap to work! 
     *)
    val freeVarMatchesAll : bool ref

    (* true if the tag in VariantType and UnionType is used to distinguish *)
    val tagMatters : bool ref
	
    (* put in ord_ket here so struct will match ORD_KEY signature *)
    type ord_key = StandardType.Typ

    (* compare closed types *)
    val compare : StandardType.Typ * StandardType.Typ -> General.order

end = 
struct
    structure TVEnv = IntBinaryMap
    structure ST = StandardType

    type ord_key = StandardType.Typ

    (* true if otherwise EQUAL types should be distinguished flow sets *)
    val checkFlowLabels = ref false	

    (* true if free type variable matches any type *)
    val freeVarMatchesAll = ref false

    (* true if the tag in VariantType and UnionType is udes to distinguish *)
    val tagMatters = ref false

    (* Get an ordering on label sets *)
    fun labelcheck (t1, t2) = raise Fail "removed"
    fun uidIsMember(term, uidList) = raise Fail "removed"

    fun type_number (ST.VarType(_)) = 0
      | type_number (ST.ConstType(_)) = 1
      | type_number (ST.ArrowType(_)) = 2
      | type_number (ST.RecordType(_)) = 3
      | type_number (ST.IsectType(_)) = 4
      | type_number (ST.VariantType(_)) = 5
      | type_number (ST.UnionType(_)) = 6
      | type_number (ST.ArrayType(_)) = 7
      | type_number (ST.RecType(_)) = 8
      | type_number (ST.AbsType(_)) = 9
      | type_number (ST.AppType(_)) = 10
      | type_number (ST.RefType(_)) = 11
      | type_number (ST.ExceptionConstructor(_)) = 12
      | type_number (ST.Exception(_)) = 13
    (* If type_number needs to be extended, then so does compare1*)
		
    (* support code for "2-finger" method, stolen from TypeChecker.sml *)
    val tyUid = ST.uidOf

    structure AssumptionMap = IntBinaryMap
    fun getAssumptions(typ,env) = (case AssumptionMap.find(env, tyUid(typ)) of
				       SOME lst => lst
				     | NONE => [])

    fun putAssumptions(typ,lst,env) = AssumptionMap.insert(env, tyUid(typ),lst)



    fun extendEnvRec(env, tvars, decls) = 
	(List.foldl 
	 (fn ((key,value),e) => TVEnv.insert(e,key,value))
	 (env)
	 (ListPair.zip(tvars, decls)))

     fun compTyEnv (envs as (env1,env2,assumpts)) =
	let
	    (* partially curried for recursive call -- compiles very slow
	    val compTySub = compTyEnv (envs)
            *)
	    (* following version of compTySub compiles fast *)
            fun compTySub (t1, t2) = compTyEnv (envs) (t1, t2) 

	    (* compare two types with 2-finger algorithm *)
	    fun compTy (ST.RecType{btvs=btvs1, decls=decls1, 
				   body=body1, ...},
			t2) =   
		(* t2 may be a RecType; will eventually be handled by 
		 * next clause *)
		(compTyEnv (* Extend env1 to associate bound type
			    * vars with declarations *)
		 (extendEnvRec(env1,btvs1,decls1), env2, assumpts)
		 (body1,t2))
		
	      | compTy (t1, ST.RecType{btvs=btvs2, decls=decls2, 
				       body=body2, ...}) =
		(* t1 is definitely not a RecType,
		 * else would have matched first clause *)	
		(compTyEnv (* Extend env2 to associate bound type 
			    * vars with declarations *)
		 (env1, extendEnvRec(env2,btvs2,decls2), assumpts)
		 (t1,body2))
		
	      (* If reach this point, guaranteed that neither type 
	       * is a RecType *)
	      | compTy (t1 as ST.VarType{...}, t2 as ST.VarType{...}) =
		compVarVar(t1,t2)
		    
	      | compTy (t1 as ST.VarType{...}, t2) = 
		(* t2 definitely not a variable type *)
		compVarNoVar(t1,t2)
			
	      | compTy (t1, t2 as ST.VarType{...}) = 
		(* t1 definitely not a variable type; 
		 * symmetric case to immediately preceding one *)
		compNoVarVar (t1, t2)

	      | compTy (t1, t2) = 
		(* 
		 * Comparing uids is a fast equality check. 
		 * Note that it is unsound to use this fast check 
		 * on variables, since it is possible for two 
		 * variable occurences to have the same uid and 
		 * refer to different types. For example, consider: 
		 *
		 *  RecType([a], [int], a)
		 *  RecType([a], [bool], a)
		 *
		 * Nothing prevents the two bodies from being the 
		 * same variable with the same uid, yet they 
		 * represent different types.
		 *) 
		if (tyUid(t1) = tyUid(t2)) then EQUAL
		else compNonVars (t1,t2)
		
	    and
		compVarVar(t1 as ST.VarType{tv=tv1, ...}, 
			   t2 as ST.VarType{tv=tv2, ...}) =
		(case (TVEnv.find(env1,tv1), TVEnv.find(env2,tv2)) of 
		     (SOME(decl1),SOME(decl2))=> compTyRemembering(decl1,decl2)
			 
		   | (SOME(decl1), _) => 
		     (* Careful! Can't just return false here.
		      * This case _can_ return 
		      * true in the following case:
		      * (o) t1 is a free type variable and t2 is 
		      * ultimately bound to it. Example:
		      *       typ1 = RecType([b],[a],b)
		      *       typ2 = a
		      * In either case need to unwind recursive type.
		      *)
			 compTyRemembering(decl1,t2)
			 
		   | (_, SOME(decl2)) => 
			 (* symmetric with previous case *)
			 compTyRemembering(t1, decl2)
			 
		   | (NONE, NONE) => 
			 (* are they the same free variable? *)
			 let val ordr = Int.compare (tv1, tv2) 
			 in 
			     if ! freeVarMatchesAll then EQUAL
			     else 
				 case ordr of 
				     EQUAL => labelcheck (t1, t2)
				   | _ => ordr
			 end
		     )
		     
	      | compVarVar (_) = raise Bind
			
	    and compVarNoVar (t1 as ST.VarType{tv=tv1, ...}, t2) = 
		(* t2 definitely not a variable type *)
		(case TVEnv.find(env1,tv1) of 
		     SOME(decl1) => 
			 (* tv1 is RecType bound, to the type decl1 *)
			 compTyRemembering(decl1,t2)
		   | NONE => if ! freeVarMatchesAll then EQUAL
			     else LESS	(* free var < any other type *)
			 )
	      | compVarNoVar (_) = raise Bind
		     
	    and compNoVarVar (t1, t2 as ST.VarType{tv=tv2, ...}) = 
		(* t1 definitely not a variable type; 
		 * symmetric with compVarNoVar *)
		(case TVEnv.find(env2,tv2) of 
		     SOME(decl2) => 
			 (* tv2 is RecType bound, to the type decl2 *)
			 compTyRemembering(t1, decl2)
		   | NONE => if ! freeVarMatchesAll then EQUAL
			     else GREATER
			 )
	      | compNoVarVar (_) = raise Bind

	    and compNonVars (t1, t2) = 
		let val ordr = Int.compare (type_number (t1), type_number (t2))
		in
		    if (ordr = EQUAL) then compare2 (t1, t2)
		    else ordr
		end

	    (* compare 2 types with same top-level constructor *)
	    and compare2 (t1 as ST.ConstType{const=consts,...},
			  t2 as ST.ConstType{const=constt,...}) =
		let
		    fun const_number (ST.IntType) = 0
		      | const_number (ST.RealType) = 1
		      | const_number (ST.CharType) = 2
		      | const_number (ST.StringType) = 3
		      | const_number (ST.UnitType) = 4
			
		    val or = Int.compare(const_number(consts),
					 const_number(constt))
		in
		    case or of 
			EQUAL => labelcheck (t1, t2)
		      | _ => or
		end

	      | compare2 (t1 as ST.ArrowType{from=froms, to=tos, ...},
			  t2 as ST.ArrowType{from=fromt, to=tot, ...}) =
		let val or = compTySub(froms, fromt)
		in
		    if (or = EQUAL) then
			let val or = compTySub(tos, tot)
			in 
			    case or of 
				EQUAL => labelcheck (t1, t2)
			      | _ => or
			end
		    else 
			or
		end
    
	      | compare2 (t1 as ST.RecordType{fields=fieldss,types=typess,...},
			  t2 as ST.RecordType{fields=fieldst,types=typest
					      ,...})=
		let val or = 
		    compareRecordIsect (fieldss, fieldst, typess, typest)
		in 
		    case or of 
			EQUAL => labelcheck (t1, t2)
		      | _ => or
		end

	    | compare2 (t1 as ST.IsectType{fields=fieldss, types=typess, ...},
			t2 as ST.IsectType{fields=fieldst, types=typest, 
					   ...}) =
		let val or = 
		    compareRecordIsect (fieldss, fieldst, typess, typest)
		in 
		    case or of 
			EQUAL => labelcheck (t1, t2)
		      | _ => or
		end


	    
	      | compare2 (t1 as ST.VariantType{tag=tagS,tags=tagss,
					       types=typess,...},
			  t2 as ST.VariantType{tag,tags=tagst,
					       types=typest,...})=
		let val or = 
		    compareVariantUnion (tagS,tag,tagss,tagst,typess,typest)
		in 
		    case or of 
			EQUAL => labelcheck (t1, t2)
		      | _ => or
		end
	    
	      | compare2 (t1 as ST.UnionType{tag=tagS,tags=tagss,
					     types=typess,...},
			  t2 as ST.UnionType{tag,tags=tagst,types=typest,...})=
		let val or = 
		    compareVariantUnion (tagS,tag,tagss,tagst,typess,typest)
		in 
		    case or of 
			EQUAL => labelcheck (t1, t2)
		      | _ => or
		end

	      | compare2 (t1 as ST.ExceptionConstructor{typ = typs, ...},
			  t2 as ST.ExceptionConstructor{typ = typt, ...}) = 
		let val or = compTySub(typs, typt)
		in 
		    case or of 
			EQUAL => labelcheck (t1, t2)
		      | _ => or
		end
	
	      | compare2 (t1 as ST.Exception(_), t2 as ST.Exception(_)) = 
		labelcheck (t1, t2)

	      | compare2 (t1 as ST.ArrayType{dims=dims, typ=typs, ...},
			  t2 as ST.ArrayType{dims=dimt, typ=typt, ...}) =
		let val or = Int.compare(dims,dimt)
		in
		    if (or = EQUAL) then 
			let val or = compTySub(typs, typt)
			in 
			    case or of 
				EQUAL => labelcheck (t1, t2)
			      | _ => or
			end
		    else or
		end
    
    
	      | compare2 (t1 as ST.RefType{typ=typs,...}, 
			  t2 as ST.RefType{typ=typt,...}) =
		let val or = compTySub(typs, typt)
		in 
		    case or of 
			EQUAL => labelcheck (t1, t2)
		      | _ => or
		end

	      | compare2 (ST.AbsType{tv=tvs, body=bodys, ...},
			  ST.AbsType{tv=tvt, body=bodyt, ...}) =
		raise (Fail ("STKey.compare didn't expect AbsType"))

	      | compare2 (ST.AppType{rator=rators, rand=rands, ...},
			  ST.AppType{rator=ratort, rand=randt, ...}) =
		    raise (Fail ("STKey.compare didn't expect AbsType"))

	      | compare2 (s, t) = 
			raise(Fail("StKey.compare2 incomparable types\n"))

	    and compareVariantUnion (tags,tagt,tagss,tagst,typess,typest) =
		if ! tagMatters then
		    let val or = String.compare(tags,tagt)
		    in 
			if (or = EQUAL) then
			    compareRecordIsect (tagss,tagst,typess,typest)
			else 
			or
		    end
		else compareRecordIsect (tagss,tagst,typess,typest)
	    
	    and compareRecordIsect (fieldss, fieldst, typess, typest) = 
		let val or = 
		    (Util.listCompare
		     (String.compare)
		     (fieldss,fieldst))

		in
		    if (or = EQUAL) then
			(Util.listCompare
			 (fn (s,t) => compTySub (s, t))
			 (typess, typest))
		    else 
			or
		end


	    and 
		(* 
		 * compTyRemembering is the version of compTy that, 
		 * in the two-finger algorithm, remembers that it has 
		 * compared these two types before. 
		 *) 
		
		compTyRemembering (ty1, ty2) =
		let 
		    val uids1 = getAssumptions(ty1,assumpts)
		(* You might think that uids2 should be treated 
		 * symmetrically.  Although this would work in the 
		 * equality case (and potentially make the algorithm 
		 * faster by reducing the amount of time for the two 
		 * fingers to match up again), it would be *incorrect*
		 * for the inequality case, which is inherently 
		 * assymmetric. 
		 * [lyn, 12/31/99]
		 *)
		in
		    if  (uidIsMember(ty2,uids1)) then 
			EQUAL	(* Have done this comparison before; 
				 * consider it true *)
		    else 
			(compTyEnv(env1, env2,
				   putAssumptions(ty1,
						  tyUid (ty2) :: uids1,
						  assumpts))
			(ty1, ty2))
		end
	in
	    compTy
	end

     (* Compare types in an empty environment (compare closed types) *)
     val compare = compTyEnv(TVEnv.empty, TVEnv.empty, AssumptionMap.empty)
	 
end					(* struct *)
