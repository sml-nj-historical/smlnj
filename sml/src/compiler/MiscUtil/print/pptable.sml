(* Copyright 1992 by AT&T Bell Laboratories *)
(* pptable.sml *)

signature PPTABLE =
sig
  exception PP_NOT_INSTALLED
  val pp_object : PrettyPrint.ppstream -> Stamps.stamp -> Unsafe.Object.object
                  -> unit
  val install_pp : string list -> 
                   (PrettyPrint.ppstream -> Unsafe.Object.object -> unit) -> unit
end

structure PPTable : PPTABLE =
struct

(* The following code implements automatic prettyprinting of values. *)
(* The user defines a datatype d, then defines a prettyprinter       *)
(*                                                                   *)
(*     dp : ppstream -> d -> unit                                    *)
(*                                                                   *)
(* over d, perhaps using the Oppen primitives. Then dp is installed  *)
(* in the "pp table" via install_pp. Subsequently, when a value of   *)
(* type d comes to be printed out, we look in the table, find dp and *)
(* apply it to the value. If it is not found, we print the value in  *)
(* the default manner.                                               *)

  type object = Unsafe.Object.object

  exception PP_NOT_INSTALLED

  fun error msg = 
        (ErrorMsg.errorNoFile (ErrorMsg.defaultConsumer(),ref false) (0,0) 
			      ErrorMsg.COMPLAIN
			      msg
			      ErrorMsg.nullErrorBody;
	 raise ErrorMsg.Error)

  val global_pp_table: (PrettyPrint.ppstream->object->unit) Stamps.stampMap =
      Stamps.newMap(PP_NOT_INSTALLED)

  fun make_path([s],p) = SymPath.SPATH(rev(Symbol.tycSymbol(s)::p))
    | make_path(s::r,p) = make_path(r,Symbol.strSymbol(s)::p)
    | make_path _ = error "install_pp: empty path" 

  fun install_pp (path_names: string list)
                 (p: PrettyPrint.ppstream -> object -> unit) =
      let val sym_path = make_path(path_names,[])
	  val tycon = Lookup.lookTyc ((#static(EnvRef.combined())),
		sym_path,
		ErrorMsg.errorNoFile(ErrorMsg.defaultConsumer(),ref false) (0,0))
       in case tycon
	    of Types.GENtyc{stamp,...} => Stamps.updateMap global_pp_table (stamp,p)
	     | _ => error "install_pp: nongenerative type constructor"
      end

  fun pp_object ppstrm (s: Stamps.stamp) (obj:object) =
      Stamps.applyMap(global_pp_table,s) ppstrm obj

end (* structure PPTABLE *)

(*
 * $Log$
 *)
