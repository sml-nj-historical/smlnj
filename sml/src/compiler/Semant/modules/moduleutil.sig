(* COPYRIGHT (c) 1996 Bell Laboratories. *)
(* moduleutil.sig *)

signature MODULEUTIL =
sig

exception Unbound of Symbol.symbol

val getSpec : Modules.elements * Symbol.symbol -> Modules.spec
val getSpecVar : Modules.spec -> EntPath.entVar option

val strDefToStr : Modules.strDef * Modules.entityEnv -> Modules.Structure

(*** getTyc, getStr and getFct are used in modules/sigmatch.sml only ***)
val getTyc : Modules.elements * Modules.entityEnv * Symbol.symbol 
                 -> Types.tycon * EntPath.entVar

val getStr : Modules.elements * Modules.entityEnv 
                * Symbol.symbol * Access.access * InlInfo.inl_info
             -> Modules.Structure * EntPath.entVar

val getFct : Modules.elements * Modules.entityEnv 
                * Symbol.symbol * Access.access * InlInfo.inl_info
             -> Modules.Functor * EntPath.entVar

(*** these functions are used in eqtypes.sml ***)
val getStrStamp : Modules.Structure -> Stamps.stamp
val getStrName : Modules.Structure -> InvPath.path
val getStrs : Modules.Structure -> Modules.Structure list
val getTycs : Modules.Structure -> Types.tycon list
val getStrSymbols : Modules.Structure -> Symbol.symbol list

(*** these functions should be called in env/lookup.sml only ***)
val getStrPath : Modules.Structure * SymPath.path * SymPath.path 
                                                         -> Modules.Structure

val getStrDef : Modules.Structure * SymPath.path * SymPath.path 
                                                         -> Modules.strDef

val getFctPath : Modules.Structure * SymPath.path * SymPath.path
                                                         -> Modules.Functor
val getTycPath : Modules.Structure * SymPath.path * SymPath.path
                                                         -> Types.tycon
val getValPath : Modules.Structure * SymPath.path * SymPath.path
                                                         -> VarCon.value

val checkPathSig : Modules.Signature * SymPath.path -> Symbol.symbol option

val eqSign : Modules.Signature * Modules.Signature -> bool
val eqOrigin : Modules.Structure * Modules.Structure -> bool

val tycId : Types.tycon -> ModuleId.modId
val strId: Modules.Structure -> ModuleId.modId
val strId2: Modules.Signature * Modules.strEntity -> ModuleId.modId
val fctId: Modules.Functor -> ModuleId.modId
val fctId2: Modules.fctSig * Modules.fctEntity -> ModuleId.modId

(*** translate tycon or type in an entityEnv ***)
val transTycon : Modules.entityEnv -> Types.tycon -> Types.tycon
val transType : Modules.entityEnv -> Types.ty -> Types.ty

(*** relativize type or tycon in an epcontext ***)
val relativizeTyc : EntPathContext.context -> Types.tycon -> Types.tycon * bool
val relativizeType : EntPathContext.context -> Types.ty -> Types.ty * bool

val openStructure : StaticEnv.staticEnv * Modules.Structure -> 
                      StaticEnv.staticEnv

(*** extract inl_info from a list of bindings *)
val extractInfo : Bindings.binding -> InlInfo.inl_info

val getSigSymbols: Modules.Signature -> Symbol.symbol list

val getSignatureNames : Modules.Structure -> Symbol.symbol list

val debugging : bool ref

end (* signature MODULEUTIL *)



(*
 * $Log: moduleutil.sig,v $
 * Revision 1.5  1997/07/15  16:13:04  dbm
 *   Changed relativizeTyc and relativizeType to return a boolean indicating
 *   whether result contains PATHtycs.
 *   Added getSignatureNames, used in build/boot.sml to extract signature
 *   names from Compile structure so they can be bound in top-level environment.
 *
 * Revision 1.4  1997/05/20  12:24:00  dbm
 *   SML '97 sharing, where structure.
 *
 * Revision 1.3  1997/03/17  18:54:21  dbm
 * Changes in datatype representation to support datatype replication.
 *
 * Revision 1.2  1997/01/21  13:25:31  george
 *    Modify the entityExp definition to correctly implement the
 *    datatype generativity in functor body. -- from zsh
 *
 *)
