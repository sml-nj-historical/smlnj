(* COPYRIGHT (c) 1998 Bell Laboratories *)
(* elabtype.sig *)

signature ELABTYPE =
sig

  val elabType :
        Ast.ty * StaticEnv.staticEnv * ErrorMsg.errorFn * SourceMap.region
        -> Types.ty * TyvarSet.tyvarset

  val elabTyvList : 
        Ast.tyvar list * ErrorMsg.errorFn * SourceMap.region 
        -> Types.tyvar list

  val elabTYPEdec :
        Ast.tb list * StaticEnv.staticEnv * InvPath.path 
        * SourceMap.region * ElabUtil.compInfo
        -> Absyn.dec * StaticEnv.staticEnv

  val elabDATATYPEdec :
        {datatycs: Ast.db list, withtycs: Ast.tb list} * StaticEnv.staticEnv 
        * ExpandTycon.sigContext * EntityEnv.entityEnv 
        * (Types.tycon -> bool) * InvPath.path 
        * SourceMap.region * ElabUtil.compInfo
        -> Types.tycon list * Types.tycon list * VarCon.datacon list 
           * StaticEnv.staticEnv

  val debugging : bool ref

end (* signature ELABTYPE *)


(*
 * $Log: elabtype.sig,v $
 * Revision 1.2  1998/05/15 03:34:04  dbm
 *   Eliminated elabDB from specs, since it is not used elsewhere.
 *
 * Revision 1.1.1.1  1998/04/08 18:39:24  george
 * Version 110.5
 *
 *)
