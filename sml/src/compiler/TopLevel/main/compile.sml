(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* compile.sml *)

local
  exception Compile of string
in

functor CompileF(structure M  : CODEGENERATOR
		 structure CC : CCONFIG) : COMPILE0 =
struct

local structure FE = FrontEnd
      structure PS = PersStamps
      structure EM = ErrorMsg
      structure SE = StaticEnv
      structure DE = DynamicEnv
      structure A  = Absyn
      structure DA = Access
      structure CB = CompBasic
      structure ST = Stats
      structure CI = Unsafe.CInterface
      structure W8V = Word8Vector
      structure V = Vector
in

val debugging = Control.CG.compdebugging
val say = Control.Print.say
fun bug s = EM.impossible ("Compile:" ^ s)
fun debugmsg msg =
  if !debugging then (say msg; say "\n"; Control.Print.flush()) else ()

exception Compile = Compile            (* raised during compilation only *)
exception SilentException = CC.SilentException     (* raised by CM *)
exception TopLevelException of exn     (* raised during executation only *)
exception TopLevelCallcc               (* raised during executation only *)
val architecture = M.architecture      (* machine architecture *)

(** important intermediate formats used during the compilations *)
type source     = CB.source            (* the input file *)
type ast        = CB.ast               (* concrete syntax *)
type absyn      = CB.absyn             (* abstract syntax *)
type flint      = CB.flint             (* intermediate code *)
type csegments  = CB.csegments         (* binary code segments *)
type executable = CB.executable        (* machine executables *)
type object     = CB.object            (* runtime object *)

(** environments and contexts used during the compilation *)
type statenv    = SE.staticEnv         (* static env   : symbol -> binding *)
type dynenv     = DE.dynenv    (* dynamic env  : pid -> object *)
type symenv     = SymbolicEnv.symenv   (* symbolic env : pid -> flint *)

type compInfo   = CB.compInfo          (* general compilation utilities *)
fun mkCompInfo (s, se, tr)  = CB.mkCompInfo (s, se, tr, CC.mkMkStamp)
val anyErrors   = CB.anyErrors

type cmstatenv  = CC.cmstatenv         (* compressed static environment *)
val toCM        = CC.toCM
val fromCM      = CC.fromCM

type lvar       = DA.lvar              (* local id *)
type pid        = PS.persstamp         (* persistant id *)
type import     = pid * CB.importTree  (* import specification *)
type pickle     = CC.pickle            (* pickled format *)
type hash       = CC.hash              (* environment hash id *)

fun fail s = raise (Compile s)

(*****************************************************************************
 *                               PARSING                                     *
 *****************************************************************************)

(** take the input source and turn it into the concrete syntax *)
val parsePhase = ST.makePhase "Compiler 010 parse"
fun parseOne (source : source) =  
  let val parser = FE.parse source
      val parser = ST.doPhase parsePhase parser (* for correct timing *)
   in fn () =>
        case parser ()
         of FE.EOF => NONE
          | FE.ABORT => fail "syntax error"
          | FE.ERROR => fail "syntax error"
          | FE.PARSE ast => SOME ast
  end

fun parse (source : source) =
  let val parser = FE.parse source
      val parser = ST.doPhase parsePhase parser (* for correct timing *)
      fun loop asts = 
	case parser()
         of FE.EOF => Ast.SeqDec(rev asts)
	  | FE.ABORT => fail "syntax error"
	  | FE.ERROR => fail "syntax error"
	  | FE.PARSE ast => loop(ast::asts)
   in loop nil
  end


(*****************************************************************************
 *                               ELABORATION                                 *
 *****************************************************************************)

(** several preprocessing phases done after parsing or after elaborations *)
(*
val fixityparse = (* ST.doPhase (ST.makePhase "Compiler 005 fixityparse") *) 
  FixityParse.fixityparse
val lazycomp = (* ST.doPhase (ST.makePhase "Compiler 006 lazycomp") *)
  LazyComp.lazycomp
*)
val pickUnpick = 
  ST.doPhase (ST.makePhase "Compiler 036 pickunpick") CC.pickUnpick

(** take ast, do semantic checks, and output the new env, absyn and pickles *)
fun elaborate {ast=ast, statenv=senv, compInfo=cinfo} = 
  let (** the following should go away soon; it needs clean up **)
      val bsenv = fromCM senv
(* lazycomp folded into elaborate phase
      val ast = fixityparse {ast=ast,env=bsenv,error=#error cinfo}
      val ast = lazycomp ast
*)
      val (absyn, nenv) = ElabTop.elabTop(ast, bsenv, cinfo)
      val (absyn, nenv) = 
        if anyErrors (cinfo) then (A.SEQdec nil, SE.empty) else (absyn, nenv)
      val {hash,pickle,exportLvars,exportPid,newenv} = pickUnpick(senv,nenv)
   in {absyn=absyn, newstatenv=toCM newenv, exportPid=exportPid, 
       exportLvars=exportLvars, staticPid = hash, pickle=pickle}
  end (* function elaborate *)

val elaborate = ST.doPhase(ST.makePhase "Compiler 030 elaborate") elaborate

(*****************************************************************************
 *                          ABSYN INSTRUMENTATION                            *
 *****************************************************************************)

(** instrumenting the abstract syntax to do time- and space-profiling *)
fun instrument {source, compInfo as {coreEnv,...}: compInfo} =
      SProf.instrumDec (coreEnv, compInfo) source 
      o TProf.instrumDec (coreEnv, compInfo)

val instrument = ST.doPhase (ST.makePhase "Compiler 039 instrument") instrument


(*****************************************************************************
 *                         TRANSLATION INTO FLINT                            *
 *****************************************************************************)

(** take the abstract syntax tree, generate the flint intermediate code *)
fun translate{absyn, exportLvars, newstatenv, oldstatenv, compInfo} =
  (*** statenv used for printing Absyn in messages ***)
  let val statenv = SE.atop (fromCM newstatenv, fromCM oldstatenv)
      val {flint, imports} = 
	    Translate.transDec(absyn, exportLvars, statenv, compInfo)
   in {flint=flint, imports=imports} 
  end

val translate = ST.doPhase (ST.makePhase "Compiler 040 translate") translate 


(*****************************************************************************
 *                         CODE GENERATION                                   *
 *****************************************************************************)

(** take the flint code and generate the machine binary code *)
local
    val inline = LSplitInline.inline
    (* fun inline (flint, imports, symenv) = flint *)
(*
    let val importExps = map (SymbolicEnv.look symenv) (map #1 imports)
     in (* optimize flint based on the knowledge of importExps *)
        bug "inline not implemented yet"
    end
*)

  fun split (flint, enable) = 
    if false (* enable *) then (case NONE (* FLINTSplit.split flint *)
                                 of NONE => (flint, NONE)
                                  | SOME x => x)
    else (flint, NONE)


  val w8vLen = W8V.length
  fun csegsize {c0, cn, data, name} =
    foldl (fn (x, y) => (w8vLen x) + y) (w8vLen c0 + w8vLen data) cn

  val addCode = ST.addStat (ST.makeStat "Code Size")
in
    fun codegen { flint: flint, imports: import list, symenv: symenv,
		  splitting: bool, compInfo: compInfo } = let
	(* hooks for cross-module inlining and specialization *)
	val (flint, revisedImports) = inline (flint, imports, symenv)
	val (flint, inlineExp : flint option) = split(flint, splitting)

	(* from optimized FLINT code, generate the machine code *)
	val csegs = M.flintcomp(flint, compInfo)
    in
	addCode(csegsize csegs); 
	{ csegments=csegs, inlineExp=inlineExp, imports = revisedImports }
    end 
end (* local codegen *)

(*
val codegen = ST.doPhase (ST.makePhase "Compiler 140 CodeGen") codegen
*)

(*****************************************************************************
 *                           COMPILATION                                     *
 *          = ELABORATION + TRANSLATION TO FLINT + CODE GENERATION           *
 * used by interact/evalloop.sml, batch/batchutil.sml, batch/cmsa.sml only   * 
 *****************************************************************************)
(** compiling the ast into the binary code = elab + translate + codegen *)
fun compile {source=source, ast=ast, statenv=oldstatenv, symenv=symenv, 
             compInfo=cinfo, checkErr=check, runtimePid=runtimePid, 
             splitting=splitting} = 
  let val {absyn, newstatenv, exportLvars, exportPid, staticPid, pickle} =
        (elaborate {ast=ast, statenv=oldstatenv, compInfo=cinfo}) 
                 before (check "elaborate")

      val absyn = 
        (instrument {source=source, compInfo=cinfo} absyn)
                 before (check "instrument")

      val {flint, imports} = 
        (translate {absyn=absyn, exportLvars=exportLvars, 
                    newstatenv=newstatenv, oldstatenv=oldstatenv, 
                    compInfo=cinfo})
              before check "translate"

      (** the following is a special hook for the case of linking the
          runtime vector when compiling PervEnv/core.sml. (ZHONG)
       *)          
      val imports =
        (case (runtimePid, imports)
          of (NONE, _) => imports
	   | (SOME p, [(_,tr)]) => [(p, tr)]
           | _ => raise Compile "core compilation failed")

      val { csegments, inlineExp, imports = revisedImports } = 
	  codegen { flint = flint, imports = imports, symenv = symenv, 
		    splitting = splitting, compInfo = cinfo }
	  before (check "codegen")
          (*
           * interp mode was currently turned off.
           *
           * if !Control.interp then Interp.interp flint
           *  else codegen {flint=flint, splitting=splitting, compInfo=cinfo})
           *)

  in
      { csegments = csegments,
        newstatenv = newstatenv,
	absyn = absyn,
	exportPid = exportPid,
	exportLvars = exportLvars,
	staticPid = staticPid,
	pickle = pickle,
	inlineExp = inlineExp,
	imports = revisedImports }
  end (* function compile *)

(*****************************************************************************
 *                        OTHER UTILITY FUNCTIONS                            *
 *****************************************************************************)

(** build the new symbolic environment *)
fun mksymenv (NONE, _) = SymbolicEnv.empty
  | mksymenv (_, NONE) = SymbolicEnv.empty
  | mksymenv (SOME pid, SOME l) = SymbolicEnv.singleton (pid, l)

(** turn the byte-vector-like code segments into an executable closure *)
local
  type w8v = W8V.vector
  val vzero = V.fromList []
  val mkCodeV : w8v * string option -> (w8v * executable) = 
        CI.c_function "SMLNJ-RunT" "mkCode"
  val mkCodeO : w8v * string option -> (w8v * (object -> object)) =
        CI.c_function "SMLNJ-RunT" "mkCode"
in
fun mkexec {c0: w8v, cn: w8v list, data : w8v, name: string option ref} =
  let val s = case !name of NONE => "EMPTY COMMENT <-- check"
                          | SOME s => s
      val nex = 
        let val (_, dt) = mkCodeV(data, NONE)
            val (_, ex) = mkCodeV(c0, SOME s)
         in fn ivec => ex (V.concat [ivec, V.fromList [dt vzero]])
        end
   in foldl (fn (c, r) => (#2 (mkCodeO (c,NONE))) o r) nex cn
  end 
end (* local *)

(** just like f x, except that it catches top-level callcc's *)
local 
  val cont_stack = ref (nil : unit ref list)
in 
fun isolate f x = (* Just like *)
  let val r = ref()
      val _ = cont_stack := r :: !cont_stack;
      fun pop_stack() =
	   case !cont_stack
	    of r' :: rest => (cont_stack := rest;
			      if r<>r' then raise TopLevelCallcc else ())
	     | _ => raise TopLevelCallcc (* can this ever happen? *)
      val a = f x 
       handle e => (pop_stack(); 
		    raise (case e of TopLevelException x => x | e => e))
   in pop_stack (); a
  end
end (* local of cont_stack *)

(*****************************************************************************
 *                        EXECUTING THE EXECUTABLE                           *
 *****************************************************************************)

(** perform the execution of the excutable, output the new dynenv *)
fun execute{executable, imports, exportPid, dynenv} = 
  let val args : object V.vector = 
        let fun selObj (obj, i) = 
              ((V.sub (Unsafe.Object.toTuple obj, i)) handle _ =>
                 bug "unexpected linkage interface in execute")

            fun getObj ((p, n), zs) = 
              let fun get (obj, CB.ITNODE [], z) = obj::z
                    | get (obj, CB.ITNODE xl, z) = 
                        let fun g ((i, n), x) = get (selObj(obj, i), n, x)
                         in foldr g z xl
                        end
                  val obj = 
                    ((DE.look dynenv p) handle DE.Unbound =>
                       (say ("lookup " ^ (PS.toHex p) ^ "\n");
                        fail "imported objects not found or inconsistent"))
               in get(obj, n, zs)
              end

         in Vector.fromList (foldr getObj [] imports)
        end
      val result : object = executable args
   in case exportPid 
       of NONE => DE.empty
	| SOME p => DE.singleton (p, result)
  end

val execute = ST.doPhase (ST.makePhase "Execute") execute

end (* local of CompileF *)
end (* functor CompileF *)

end (* local of exception Compile *)


(*
 * $Log: compile.sml,v $
 * Revision 1.5  1998/06/02 17:39:29  george
 *   Changes to integrate CM functionality into the compiler --- blume
 *
 *)
