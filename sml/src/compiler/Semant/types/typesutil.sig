(* Copyright 1996 by AT&T Bell Laboratories *)
(* typesutil.sig *)

signature TYPESUTIL = 
sig

  structure Types : TYPES

  val eqpropToString : Types.eqprop -> string

  (* operations to build tyvars, VARtys *)
  val mkMETA : int -> Types.tvKind
  val mkFLEX : ((Symbol.symbol * Types.ty) list) * int -> Types.tvKind
  val mkUBOUND : Symbol.symbol -> Types.tvKind
  val mkLITERALty : Types.litKind * SourceMap.region -> Types.ty
  val mkSCHEMEty : unit -> Types.ty
  val mkMETAty : unit -> Types.ty
  val mkMETAtyBounded : int -> Types.ty

  (* primitive operations on tycons *)
  val tycName : Types.tycon -> Symbol.symbol
  val tycStamp : Types.tycon -> Stamps.stamp
  val tycPath : Types.tycon -> InvPath.path

  val tycEntPath : Types.tycon -> EntPath.entPath
  val tyconArity : Types.tycon -> int
  val setTycPath : Types.tycon * InvPath.path -> Types.tycon
  val eqTycon : Types.tycon * Types.tycon -> bool
  val mkCONty : Types.tycon * Types.ty list -> Types.ty

  val prune : Types.ty -> Types.ty

  val eqTyvar : Types.tyvar * Types.tyvar -> bool
  val bindTyvars : Types.tyvar list -> unit
  val bindTyvars1 : Types.tyvar list -> Types.polysign
    
  exception ReduceType
  val mapTypeFull: (Types.tycon -> Types.tycon) -> Types.ty -> Types.ty
  val applyTyfun : Types.tyfun * Types.ty list -> Types.ty
  val reduceType : Types.ty -> Types.ty
  val headReduceType : Types.ty -> Types.ty
  val equalType  : Types.ty * Types.ty -> bool
  val equalTycon : Types.tycon * Types.tycon -> bool

  (* making a "generic" copy of a type *)
  val typeArgs : int -> Types.ty list
  val mkPolySign : int -> Types.polysign
  
  val dconTyc : Types.datacon -> Types.tycon
  val dconType : Types.tycon * Types.ty option  -> Types.ty

  (* matching a scheme against a target type -- used declaring overloadings *)
  val matchScheme : Types.tyfun * Types.ty -> Types.ty

  (* get rid of INSTANTIATED indirections in a type *)
  val compressTy : Types.ty -> unit  

  type occ
  val Abstr : occ -> occ
  val LetDef: occ -> occ
  val Root : occ
  val lamdepth : occ -> int
  val toplevel : occ -> bool

  val instantiatePoly : Types.ty -> Types.ty * Types.ty list

  val compareTypes : Types.ty * Types.ty -> bool 

  val tyvarType : Types.ty -> Types.tyvar

  (* 
   * Check if a bound tyvar has occurred in some datatypes, e.g. 'a list. 
   * this is useful for representation analysis; but it should be 
   * obsolete very soon -- zsh. 
   *)
  val getRecTyvarMap : int * Types.ty -> (int -> bool)
  val gtLabel : Symbol.symbol * Symbol.symbol -> bool

  val isValue : Absyn.exp -> bool
  val isVarTy : Types.ty -> bool

  val sortFields : (Absyn.numberedLabel * 'a) list
        -> (Absyn.numberedLabel * 'a) list
  val mapUnZip : ('a -> 'b * 'c) -> 'a list -> 'b list * 'c list

  type tycset
  val mkTycSet : unit -> tycset
  val addTycSet : Types.tycon * tycset -> tycset
  val filterSet : Types.ty * tycset -> Types.tycon list

  val reformat : (Types.ty * Types.tycon list * DebIndex.depth) ->
                    Types.ty * PLambdaType.tkind list * Types.tycpath list

  val dtSibling : int * Types.tycon -> Types.tycon
  val extractDcons: Types.tycon -> Types.datacon list

  val wrapDef : Types.tycon * Stamps.stamp -> Types.tycon
      (* make a tycon into a DEFtyc by "eta-expanding" if necessary *) 

  val unWrapDef1 : Types.tycon -> Types.tycon option
  val unWrapDefStar : Types.tycon -> Types.tycon

  val dummyTyGen : unit -> unit -> Types.ty
      (* create dummy type generators used to instantiate ungeneralizable
       * free type variables in Typechecking.generalizeTy *)

end  (* signature TYPESUTIL *)

(*
 * $Log: typesutil.sig,v $
 * Revision 1.1.1.1  1998/04/08 18:39:36  george
 * Version 110.5
 *
 *)
