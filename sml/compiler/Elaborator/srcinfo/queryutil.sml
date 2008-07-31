(* queryutil.sml *)

structure QueryUtil =
struct

(* query utility functions *)

open DBTypes Database

structure S = Symbol
structure A = Access

fun bug msg = ErrorMsg.impossible("Bugs in QueryUtil: "^msg);


(* test whether an occurrence is a use of a variable given by a var_elem 
 * -- this assumes that the occurrence argument is an applied occurrence *)
fun occIsVar (occ as (sym,loc): occurrence) ({name,usage,...} : var_elem) : bool =
    S.eq (sym,name) andalso List.exists (fn (loc',_,_) => eqLocation(loc', loc)) (!usage)

fun findUse(loc: location, usage: varUse list) : varUse option =
    List.find (fn (loc',ty,acc) => eqLocation(loc,loc')) usage


fun toVarOcc (s, loc) = 
    ((Symbol.varSymbol s), loc)

type str_context = str_elem -> bool

fun search(sym, []) = NONE
  | search(sym, (_,sym',key)::rest) = 
    if S.eq(sym,sym') then SOME key else search(sym,rest)

fun str_has_key (acc,filepath) ({access,...}:str_elem) = 
    case compare_acc (acc,access)
     of EQUAL => true
      | _ => false

fun var_has_key (acc,filepath) ({access,...}:var_elem) = 
    case compare_acc (acc,access)
     of EQUAL => true
      | _ => false

fun type_has_key s ({stamp,...}: type_elem) = 
    Stamps.eq (s,stamp)

fun cons_has_key (stamp,sym) ({name,dataty,...}:cons_elem) = 
    Stamps.eq (stamp,dataty) andalso S.eq (sym,name)

fun sig_has_key s ({stamp,...}: sig_elem) = 
    Stamps.eq (s, stamp)

fun fileOf ({def,...}: str_elem) = locFile def

fun getSlotElements (Def elems, slot) : key =
      #3(List.nth(elems,slot))
  | getSlotElements (Constraint(elems,acc), slot) =
      let val (_,_,slot') = List.nth(elems,slot)
      in case find_str(str_has_key (acc,"???"))
	  of SOME {elements,...} => getSlotElements(elements,slot')
	   | NONE => bug ""
      end
  | getSlotElements (Alias acc, slot) =
    (case find_str(str_has_key (acc,"???"))
      of SOME {elements,...} => getSlotElements(elements,slot)
       | NONE => bug "")

fun transAccFile get_lvar (af as (acc: A.access, filepath: string)) : (A.access * string) =
    case acc
      of A.LVAR _ => af  (* access is local to filepath *)
       | A.PATH _ => (* ASSERT: access must be external rooted *)
	 let val (acc',filepath') = localize_access acc
	 in (get_lvar filepath' acc', filepath')
	 end
       | _ => bug "transAccFile"

(* find : str_elem * symbol list * string -> location option *)
fun find(strelem,sympath,filepath) : location option =
    let val {elements,def,...} = strelem
    in  case sympath
	 of [] => SOME def
	  | [sym] => 
	    (case elements
	      of Def elems =>
		 (case search(sym,elems)
		   of SOME key =>
		      (case key
			of Str acc =>
			   (case find_str(str_has_key (transAccFile get_str_lvar_g (acc, filepath)))
			     of SOME{def,...} => SOME def
			      | NONE => NONE)
			 | Var acc =>
			   (case find_var(var_has_key (transAccFile get_var_lvar_g (acc, filepath)))
			     of SOME{def,...} => SOME def
			      | NONE => NONE)
			 | Type stamp =>
			   (case find_typ(type_has_key (stamp))
			     of SOME{def,...} => SOME def
			      | NONE => NONE)
			 | Cons (stamp,name) =>
			   (case find_cons(cons_has_key (stamp,name))
			     of SOME{def,...} => SOME def
			      | NONE => NONE)
			 | Sig stamp =>
			   (case find_sig(sig_has_key (stamp))
			     of SOME{def,...} => SOME def
			      | NONE => NONE)
		      )
		    | NONE => NONE)
	       | Constraint _ => NONE
	       | Alias _ => NONE)
	  | next::rest =>
	    (case elements
	      of Def elems => 
		 (case search(next,elems)
		   of SOME (Str acc) => 
		      let val (acc',filepath') = transAccFile get_str_lvar_g (acc,filepath)
		      in case find_str(str_has_key (acc',filepath'))
			  of SOME strelem' => find (strelem',rest,filepath')
			   | NONE => bug "find-Def"
		      end
		    | _ => bug "")
	       | Constraint (elems,acc) => 
		 (case search(next,elems)
		   of SOME slot =>
		      let val (acc',filepath') = transAccFile get_str_lvar_g (acc,filepath)
		      in case find_str(str_has_key (acc',filepath'))
			  of SOME{elements,...} =>
			     let val Str acc'' = getSlotElements (elements,slot)
				 val (acc''',filepath'') = transAccFile get_str_lvar_g (acc'',filepath')
			     in case find_str(str_has_key (acc''',filepath''))
				  of SOME strelem' =>
				     find (strelem',rest,filepath'')
				   | NONE => bug "find-Constrain"
			     end
			   | NONE => NONE
		      end
		    | NONE => NONE)
	       | Alias acc => 
		 let val (acc',filepath') = transAccFile get_str_lvar_g (acc,filepath)
		  in case find_str(str_has_key (acc',filepath'))
		      of SOME strelem'' => find(strelem'',rest,filepath')
		       | NONE => bug "find=Alias"
		 end)
    end

end (* structure QueryUtil *)
