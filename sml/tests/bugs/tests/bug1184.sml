(* bug1184.sml *)

(* inlined version of Mil to ensure that functors aren't the problem *)
structure Mil = struct
  type tc = int
  type tv = int
  type pv = int

  datatype texp =
      Typevar of tv
    | Funtype of texp list * texp
    | Typeapp of tc * texp list

  datatype tscm =
      Tscm of tv list * texp
	
  datatype export = 
      Type of texp * string
    | Name of (pv * texp) * string

  datatype desc = 
      Enum
    | Value
    | Record
    | VarRec
    | OneNull of pv * desc
    | FlatRec
    | FlatVarRec

  datatype alg = 
      Conc of tc * tv list * prim list
    | Term of tc * tv list * variant list * desc
      
  and prim = Prim of pv * texp

  and variant = Variant of pv * texp * pv list
    
  datatype vdec = Vdec of tscm * pv

  datatype fdec = Fdec of tscm * pv * pv list * stmnt

  and ldec = Ldec of pv * stmnt

  and stmnt =
      Halt
    | Return of exp
    | Goto of pv
    | Assign of pv * rhs * stmnt
    | Case of desc * pv * calt list
    | Block of vdec list * fdec list * ldec list * stmnt

  and rhs = 
      Exp of exp
    | Papp of pv * exp list
    | App of pv * exp list
    | Cata of tc * pv * exp * exp

  and exp =
      Const of texp * string
    | Var of texp * pv
    | Dref of texp * desc * pv * pv * pv

  and calt = Calt of pv * stmnt

  datatype module =
      Module of (export list * alg list list * vdec list * fdec list * stmnt)
end

structure MilC = struct
  (* Simplified stuff from other modules *)
  (* Standard *)
  exception StdExcept of string * string
  fun fail s = raise (StdExcept ("MilC",s))
  fun comma l = ""
  fun unzip l = ([],[])
  fun eqmember x [] = false
    | eqmember x (y::l) = if x = y then true else eqmember x l
  fun map2 f [] [] = []
    | map2 f (x::xs) (y::ys) = (f (x,y))::(map2 f xs ys)
    | map2 _ _ _ = fail "map2"
  (*
  fun eqmember x l = false
  fun map2 f xs ys = []
  *)
  (* IAE *)
  fun empty () = []
  fun extend s nvs = s
  fun apply s n = NONE
  (* CLib *)
  datatype ppi = IN of int | UN | LI of string list
  fun sepflat delim l = []
  fun sep delim l = []
  fun sym2str b i = Int.toString i

  (* Original code starts here *)
  open Mil
  fun pv_error pv = fail ("unknown variable "^(sym2str true pv))
  fun mono_error x = fail ("generics encountered translating a "^x)
  fun ho_error x =  fail ("higher-order type encountered translating a "^x)

  fun get_tc tc = sym2str true tc
  fun get_pv x = sym2str true x
  val l2str = get_pv
  val (exported_pvs,imported_pvs) = ([],[])
  fun is_imported_pv pv = eqmember pv imported_pvs
  fun is_exported_pv pv = eqmember pv exported_pvs

  fun codegen (width:int) (packageName:string) template (module:Mil.module) =
      let
	fun mil2c_texp texp = 
	    case texp of
		Typeapp (tc,[]) => get_tc tc
	      | Funtype (texps,texp) => ho_error "texp"
	      | _ => mono_error "texp"

	fun mil2c_params xs texps = ["("] @ (sepflat "," (map2 mil2c_param xs texps)) @ [")"]

	and mil2c_param (x,texp) : string list = [mil2c_texp texp," ",get_pv x]

	fun spec_algsl [] = ([],[],[])
	  | spec_algsl (algs::algsl) =
	    let val (type_dec,type_def,cv_defs,conv_defs) = spec_algs algs
		val (type_decs,cv_defs',conv_defs') = spec_algsl algsl
	    in (type_dec@type_def@type_decs,cv_defs@cv_defs',conv_defs@conv_defs') end

	and spec_algs algs = foldr spec_alg ([],[],[],[]) algs

	and spec_alg (alg,res) = res

	and spec_con texps texp f xs  = [LI ([mil2c_texp texp," ",(get_pv f)," "] @ (mil2c_params xs texps) @ [";"])]

	and spec_term_alg ((tc,variants,desc),(type_decs,type_defs,cv_defs,conv_defs)) =
	    (type_decs,type_defs,cv_defs,conv_defs)

	and spec_varrecs tc variants =
	    let val tc' = get_tc tc
		val c_variants = map (fn (Variant (c,_,_)) => (get_pv c)^"_variant") variants
		val tc_dec = 
		    [LI ["typedef enum {",(comma c_variants),"} ",tc' ^ "_constructors;"],
		     LI ["typedef struct "^tc'^"_item *"^tc'^";"]]
		val (tc_bodies,cv_defs) = unzip (map2 (spec_varrec tc') c_variants variants)
		val tc_def = List.concat tc_bodies
	    in (tc_dec,tc_def,List.concat cv_defs)  end

	and spec_varrec tc (c',Variant (c,vtexp,ls)) =
	    case vtexp of 
		Funtype(texps,texp) =>
		    let fun make_def (l,texp) = 
			[LI [(mil2c_texp texp)," ",(l2str l),";"]]
			val body = List.concat (map2 make_def ls texps)
			val cv_def = if is_exported_pv c then spec_con texps texp c ls else []
		    in (body,cv_def)
		    end
	      | _ => let val body = [LI ["struct {} ",(get_pv c),";"]]
			 val cv_def = 
			     [LI ["static struct ",tc ^ "_item ",c' ^ "_item","= {",c',"};"],
			      LI ["static ",tc," ",get_pv c,"= &",c' ^ "_item;"]]
		     in (body,cv_def) end 

	and spec_flatvarrecs tc variants =
	    let val tc' = get_tc tc
		val c_variants = map (fn (Variant (c,_,_)) => (get_pv c)^"_variant") variants
		val tc_dec = 
		    [ LI ["typedef enum {",(comma c_variants),"} ",tc' ^ "_constructors;"]]
		val (tc_bodies,cv_defs)= unzip (map2 (spec_flatvarrec tc') c_variants variants)
		val tc_def = List.concat tc_bodies
	    in (tc_dec,tc_def,List.concat cv_defs) end

	and spec_flatvarrec tc (c',Variant (c,vtexp,ls)) =
	    case vtexp of 
		Funtype(texps,texp) =>
		    let fun make_def (l,texp) = [LI [(mil2c_texp texp)," ",(l2str l),";"]]
			val body = 
			    [LI ["struct {"],
			     IN 2]
			    @ (List.concat (map2 make_def ls texps))
			    @ [UN,
			       LI ["} ",(get_pv c),";"]]
			val cv_def = if is_exported_pv c then spec_con texps texp c ls else []
		    in (body,cv_def) 
		    end
	      | _ => let val body = [LI ["struct {} ",(get_pv c),";"]]
			 val cv_def =  
			     [LI ["static ",mil2c_texp vtexp," ",get_pv c,"= {",c',"};"]]
		     in (body,cv_def)
		     end

       fun mil2c_algsl pair [] = pair
	 | mil2c_algsl pair (algs::algsl) =
	   mil2c_algsl (mil2c_algs pair algs) algsl

       and mil2c_algs pair [] = pair
	 | mil2c_algs pair (alg::algs) =
	   mil2c_algs (mil2c_alg pair alg) algs

       and mil2c_alg pair alg =
	   case alg of 
	       Term (tc,[],[variant],Value) => mil2c_value pair (get_tc tc) variant
	     | _ => pair

       and mil2c_value (pair as (cons_defs,pdfe)) tc (Variant (c,Funtype ([texp],_),[l])) =
	   let val c' = get_pv c
	       val l' = get_pv l
	       val cons_def = 
		   if not (is_exported_pv c) then [] else [] 
	       fun cpdf (x,vs) = [] 
	   in (cons_def@cons_defs,extend pdfe [(c,cpdf)])
	   end
	 | mil2c_value pair tc variant = pair

       fun mil2c_global_vdecs vdecs = 
	   let val (glo,loc) = foldr mil2c_global_vdec ([],[]) vdecs
	   in ([ LI (List.concat glo)],[ LI (List.concat loc)]) end

       and mil2c_global_vdec (Vdec (Tscm ([],texp),x),(glo,loc)) =
	   if is_exported_pv x then  ([mil2c_texp texp," ",get_pv x,";"]::glo,loc)
	   else (glo,["static ",mil2c_texp texp," ",get_pv x,";"]::loc)
	 | mil2c_global_vdec vdec = fail "bad var declaration"
	   
       fun mil2c_vdecs vdecs = 
	   [LI (List.concat (foldr mil2c_vdec [] vdecs)) ]
	   
       and mil2c_vdec (Vdec (Tscm ([],texp),x),res) =
	   if is_imported_pv x then res 
	   else ([mil2c_texp texp," ",get_pv x,";"])::res
	 | mil2c_vdec _ = fail "bad var declaration"
	   
       fun spec_global_fdecs fdecs = ([],[])

       and spec_global_fdec (Fdec (Tscm ([],Funtype (texps,texp)),f,xs,stmnt),(glo,loc)) =
	   if is_exported_pv f
	       then (LI ([mil2c_texp texp," ",get_pv f," "] @ (mil2c_params xs texps)@[";"])::glo,loc)
	   else (glo,LI (["static ",mil2c_texp texp," ",get_pv f," "] @ (mil2c_params xs texps)@[";"])::loc)
	 | spec_global_fdec fdec = fail "bad function declaration"

       fun mil2c_fdecs pdfe fdecs = (pdfe,[])

       fun mil2c_module pdfe (Module (exports,algsl,vdecs,fdecs,stmnt)) = 
	   let val (type_decs,cv_defs,conv_defs) = spec_algsl algsl
	       val (cons_defs,pdfe') = mil2c_algsl ([],pdfe) algsl
	       val (global_vdefs,local_vdefs) = mil2c_global_vdecs vdecs
	       val (global_f_specs,local_f_specs) = spec_global_fdecs fdecs
	       val (pdfe'',f_defs) = mil2c_fdecs pdfe' fdecs
	       val body = [] 
	   in List.concat
	       [type_decs,conv_defs,cv_defs,global_vdefs,global_f_specs,
		cons_defs,local_vdefs,local_f_specs,f_defs,body]
	   end

       val pb = mil2c_module [] module
    in pb end
end
