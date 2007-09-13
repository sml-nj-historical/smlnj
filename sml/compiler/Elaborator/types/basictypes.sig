(* Copyright 1996 by AT&T Bell Laboratories *)
(* basictypes.sig *)
 
signature BASICTYPES =
sig

  val arrowStamp : Stamps.stamp
  val arrowTycon : Types.tycon 
  val --> : Types.ty * Types.ty -> Types.ty
  val isArrowType : Types.ty -> bool
  val domain : Types.ty -> Types.ty
  val range : Types.ty -> Types.ty

  val intTycon : Types.tycon 
  val intTy : Types.ty

  val int32Tycon : Types.tycon 
  val int32Ty : Types.ty

  val int64Tycon : Types.tycon
  val int64Ty : Types.ty

  val intinfTycon : Types.tycon
  val intinfTy : Types.ty

  val realTycon  : Types.tycon 
  val realTy : Types.ty

  val wordTycon : Types.tycon 
  val wordTy : Types.ty

  val word8Tycon : Types.tycon
  val word8Ty: Types.ty

  val word32Tycon : Types.tycon
  val word32Ty: Types.ty

  val word64Tycon : Types.tycon
  val word64Ty : Types.ty

  val stringTycon  : Types.tycon 
  val stringTy : Types.ty

  val charTycon  : Types.tycon 
  val charTy : Types.ty

  val exnTycon : Types.tycon 
  val exnTy : Types.ty

  val contTycon : Types.tycon 
  val ccontTycon : Types.tycon 

  val arrayTycon : Types.tycon 

  val vectorTycon : Types.tycon

  val objectTycon : Types.tycon
  val c_functionTycon : Types.tycon
  val word8arrayTycon : Types.tycon
  val real64arrayTycon : Types.tycon
  val spin_lockTycon : Types.tycon

  val unitTycon : Types.tycon 
  val unitTy : Types.ty

  val recordTy : (Types.label * Types.ty) list -> Types.ty
  val tupleTy : Types.ty list -> Types.ty
  (* get the types of a tuple-type's fields *)
  val getFields : Types.ty -> Types.ty list option

  val boolTycon : Types.tycon 
  val boolTy : Types.ty
  val boolsign : Access.consig
  val falseDcon : Types.datacon
  val trueDcon : Types.datacon

  (*  
   *  Unnecessary; removed by appel
   *  val optionTycon : Types.tycon 
   *  val NONEDcon : Types.datacon
   *  val SOMEDcon : Types.datacon
   *)

  val refTycon : Types.tycon 
  val refPatType : Types.ty
  val refDcon : Types.datacon

  val listTycon : Types.tycon 
  val nilDcon : Types.datacon
  val consDcon : Types.datacon

  val ulistTycon : Types.tycon 
  val unilDcon : Types.datacon
  val uconsDcon : Types.datacon

  val fragTycon : Types.tycon
  val ANTIQUOTEDcon : Types.datacon
  val QUOTEDcon : Types.datacon

  val suspTycon : Types.tycon       
  val suspPatType : Types.ty        
  val dollarDcon : Types.datacon    

end (* signature BASICTYPES *)
