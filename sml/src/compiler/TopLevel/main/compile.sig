(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* compile.sig *)

(*
 * Trimmed to contain only compile-related stuff but no linking or execution.
 *   -- 07/18/2001 (blume)
 *)

signature COMPILE0 = sig

    type pickle				(* pickled format *)
    type hash				(* environment hash id *)
    type pid = PersStamps.persstamp

    val mkCompInfo :
	{ source: Source.inputSource, transform: Absyn.dec -> Absyn.dec }
	-> Absyn.dec CompInfo.compInfo

    (** take ast, do semantic checks,
     ** then output the new env, absyn and pickles *)
    val elaborate : { ast: Ast.dec,
		      statenv: StaticEnv.staticEnv,
		      compInfo: Absyn.dec CompInfo.compInfo,
		      uniquepid: hash -> hash * string }
                    -> { absyn: Absyn.dec,
			 newstatenv: StaticEnv.staticEnv,
 			 exportLvars: Access.lvar list,
			 exportPid: pid option,
			 staticPid: hash,
			 fingerprint: hash,
			 pepper: string,
			 pickle: pickle }

    (** elaborate as above, then keep on to compile into the binary code *)
    val compile : { source: Source.inputSource,
		    ast: Ast.dec,
		    statenv: StaticEnv.staticEnv,
                    symenv: SymbolicEnv.env,
		    compInfo: Absyn.dec CompInfo.compInfo, 
		    uniquepid: hash -> hash * string,
                    checkErr: string -> unit,
                    splitting: int option}
                  -> { csegments: CodeObj.csegments,
		       newstatenv: StaticEnv.staticEnv,
                       absyn: Absyn.dec (* for pretty printing only *),
                       exportPid: pid option,
		       exportLvars: Access.lvar list,
                       staticPid: hash,
		       fingerprint: hash,
		       pepper: string,
		       pickle: pickle, 
                       inlineExp: FLINT.prog option,
		       imports: ImportTree.import list }

end (* signature COMPILE0 *)

signature COMPILE = COMPILE0 where type pickle = Word8Vector.vector
                               and type hash = PersStamps.persstamp

signature TOP_COMPILE = COMPILE0 where type hash = unit
