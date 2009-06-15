(* elabcontrol.sig
 *
 * (C) SML Fellowship
 *)

signature ELAB_CONTROL =
sig

  val etdebugging : bool ref
      (* ElabType *)
  val esdebugging : bool ref
      (* ElabSig *)
  val insdebugging : bool ref
      (* Instantiate *)
  val smdebugging : bool ref
      (* Sigmatch *)
  val ecdebugging : bool ref
      (* ElabCore *)
  val emdebugging : bool ref
      (* ElabMod *)
  val tcdebugging : bool ref
      (* Typecheck *)
  val unidebugging : bool ref
      (* Unify *)
  val instantiateSigs : bool ref
      (* ElabMod, Control_MC *)

  val internals : bool ref

  val markabsyn : bool ref
      (* ElabCore, ElabTop, ElabUtil, Control_MC *)                    

  val boxedconstconreps : bool ref
      (* ConRep *)

  val multDefWarn : bool ref
      (* Instantiate, Control_MC (TopLevel/main/control.sml) *)

  val shareDefError : bool ref
      (* Instantiate, Control_MC *)

  val valueRestrictionLocalWarn : bool ref

  val valueRestrictionTopWarn : bool ref

end (* signature ELAB_CONTROL *)
