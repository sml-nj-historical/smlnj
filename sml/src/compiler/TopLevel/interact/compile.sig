(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* compile.sig *)

signature COMPILE0 = 
sig

exception Compile of string
type lvar = Lambda.lvar
type absyn
type compInfo 

type lambda = Lambda.lexp
type pid = PersStamps.persstamp
type obj = Unsafe.Object.object
type csegments = {c0: Word8Vector.vector, cn: Word8Vector.vector list, name:string option ref}

val debugging : bool ref

val mkCompInfo : Source.inputSource * StaticEnv.staticEnv *
                 (Absyn.dec -> Absyn.dec) -> compInfo
val anyErrors: compInfo -> bool

val parse: Source.inputSource -> Ast.dec
val parseOne: Source.inputSource -> unit -> Ast.dec option

structure SCS : sig type staticEnv
		    val SC : StaticEnv.staticEnv->staticEnv
		    val unSC : staticEnv->StaticEnv.staticEnv
		end
type pickle
type hash

val fixityparse:  {ast: Ast.dec,
            compenv: StaticEnv.staticEnv,
            compInfo: compInfo} -> 
                  {ast: Ast.dec,
            compenv: StaticEnv.staticEnv,
            compInfo: compInfo}

val lazycomp:  {ast: Ast.dec,
            compenv: StaticEnv.staticEnv,
            compInfo: compInfo} ->
               {ast: Ast.dec}

val elaborate: {ast: Ast.dec,
		compenv: SCS.staticEnv,
		compInfo: compInfo}
                -> {absyn: Absyn.dec, 
	            newenv: SCS.staticEnv,
 	            exportLvars: lvar list,
	            exportPid: pid option,
		    staticPid : hash,
		    pickle : pickle}

val makePid:  SCStaticEnv.staticEnv * SCStaticEnv.staticEnv 
              -> PersStamps.persstamp

val instrument: {source: Source.inputSource,
		 compenv: SCS.staticEnv,
		 compInfo: compInfo} -> Absyn.dec -> Absyn.dec

val translate: {absyn: Absyn.dec,
                exportLvars: lvar list,
                exportPid: pid option,
                newstatenv : SCS.staticEnv,
                oldstatenv: SCS.staticEnv,
		compInfo: compInfo} 
                -> {genLambda: lambda option list -> lambda,
                    imports: pid list}

val symDelta: pid option * lambda option -> SymbolicEnv.symenv
val architecture : string

(* perform the inlining *)
val inline: {genLambda: lambda option list -> lambda,
             imports: pid list, symenv: SymbolicEnv.symenv} -> lambda

(* Split code in expansive code segments and an (optional) inlinable part *)
val split: {lambda: lambda, enable: bool} 
               -> {lambda_e: lambda, lambda_i: lambda option}

val codegen: {lambda: lambda, compInfo: compInfo} -> csegments

val applyCode : csegments -> obj vector -> obj


(* the functions above raise ONLY the exception Compile;
   execute can raise other exceptions *)

(* Isolate can raise ONLY TopLevelException or TopLevelCallcc*)

exception TopLevelException of exn
exception SilentException		(* raised by CM *)
exception TopLevelCallcc

val isolate : ('a -> 'b) -> 'a -> 'b

val execute: {executable: obj vector -> obj,
              imports: pid list,
              exportPid: pid option,
              dynenv: DynamicEnv.dynenv}
              -> DynamicEnv.dynenv  (* new "delta" dynenv *)

end (* signature COMPILE *)

signature COMPILE = COMPILE0 where type SCS.staticEnv=SCStaticEnv.staticEnv
signature TOP_COMPILE = COMPILE0 where type SCS.staticEnv=StaticEnv.staticEnv

(*
 * $Log: compile.sig,v $
 * Revision 1.7  1997/09/22  17:36:37  appel
 * Eliminate build/topcompile.sml by merging it with build/compile.sml
 *
 * Revision 1.6  1997/08/25  19:20:02  riccardo
 *   Added support for tagging code objects with their source/bin file name.
 *
 * Revision 1.5  1997/08/02  02:08:36  dbm
 *   New top level.  mkCompInfo now takes coreEnv : StaticEnv.staticEnv.
 *
 * Revision 1.4  1997/06/30  19:36:59  jhr
 *   Removed System structure; added Unsafe structure.
 *
 * Revision 1.3  1997/05/05  19:55:02  george
 *    Turning off some measurement hooks - zsh
 *
 * Revision 1.2  1997/03/25  13:41:42  george
 *   Fixing the coredump bug caused by duplicate top-level declarations.
 *   For example, in almost any versions of SML/NJ, typing
 *           val x = "" val x = 3
 *   would lead to core dump. This is avoided by changing the "exportLexp"
 *   field returned by the pickling function (pickle/picklemod.sml) into
 *   a list of lambdavars, and then during the pretty-printing (print/ppdec.sml),
 *   each variable declaration is checked to see if it is in the "exportLvars"
 *   list, if true, it will be printed as usual, otherwise, the pretty-printer
 *   will print the result as <hiddle-value>.
 * 						-- zsh
 *
 *)
