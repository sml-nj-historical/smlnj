(* COPYRIGHT (c) 1998 YALE FLINT PROJECT *)
(* flintcomp.sml *)

functor FLINTComp (structure Gen: CPSGEN
                   val collect: unit -> Word8Vector.vector) : CODEGENERATOR =
struct

local structure CB = CompBasic
      structure CGC = Control.CG
      structure MachSpec = Gen.MachSpec
      structure Convert = Convert(MachSpec)
      structure CPStrans = CPStrans(MachSpec)
      structure CPSopt = CPSopt(MachSpec)
      structure NewClosure = NClosure(MachSpec)
      structure Spill = Spill(MachSpec)
      structure CpsSplit = CpsSplitFun (MachSpec) 
in 

val architecture = Gen.MachSpec.architecture
fun bug s = ErrorMsg.impossible ("FLINTComp:" ^ s)
val say = Control.Print.say

fun phase x = Stats.doPhase (Stats.makePhase x)

val lconLexp  = phase "Compiler 052 lcontract" LContract.lcontract 
val specLexp  = phase "Compiler 053 specLexp" Specialize.specialize
val wrapLexp  = phase "Compiler 054 wrapLexp" Wrapping.wrapLexp
val wrapLexpN = phase "Compiler 054 wrapLexpN" WrappingNEW.wrapping
val ltyComp   = phase "Compiler 055 ltyComp" Reify.ltyComp
val reify     = phase "Compiler 055 ltyCompN" ReifyNEW.reify
val narrow    = phase "Compiler 056 ltNarrow" LtNarrow.narrow
(* val lambdaopt = phase "Compiler 057 lambdaopt" LContract.lcontract *)

val convert   = phase "Compiler 060 Convert" Convert.convert
val cpstrans  = phase "Compiler 065 CPStrans" CPStrans.cpstrans
val cpsopt    = phase "Compiler 070 cpsopt" CPSopt.reduce
val closure   = phase "Compiler 080 closure"  NewClosure.closeCPS
val globalfix = phase "Compiler 090 globalfix" GlobalFix.globalfix
val spill     = if MachSpec.spillAreaSz < 500 * MachSpec.valueSize
                then phase "Compiler 100 spill" Spill.spill
                else fn x => x
val limit     = phase "Compiler 110 limit" Limit.nolimit
val codegen   = phase "Compiler 120 cpsgen" Gen.codegen

fun prGen (flag,printE) s e =
  (if !flag then (say ("\n\n[After " ^ s ^ " ...]\n\n"); printE e) else ();
   e)

val prLexp  = prGen (CGC.printLambda, MCprint.printLexp)
val prFlint = prGen (CGC.printLambda, PPFlint.printProg)
val prCps   = prGen (CGC.printit, PPCps.printcps0)

(** compiling FLINT code into the binary machine code *)
fun flintcomp(flint, compInfo as {error, sourceName=src, ...}: CB.compInfo) = 
  let fun err severity s =
 	error (0,0) severity (concat["Real constant out of range: ",s,"\n"])

      fun dumpTerm (printE, s, le) =
	let val outS = TextIO.openAppend (src ^ s);
	    val saveOut = !Control.Print.out
	    fun done () =
		(TextIO.closeOut outS; Control.Print.out := saveOut)
	 in Control.Print.out := {say = fn s => TextIO.output(outS,s),
				  flush = fn () => TextIO.flushOut outS};
	    printE le handle x => (done () handle _ => (); raise x);
	    done ()
	end
      fun check (checkE,printE,chkId) (enableChk,lvl,logId) e =
	(if !enableChk andalso checkE (e,lvl) then
	   (dumpTerm (printE, "." ^ chkId ^ logId, e);
	    bug (chkId ^ " typing errors " ^ logId))
	 else ();
	 e)
      val chkLexp = check (CheckLty.checkLty, MCprint.printLexp, "lambda")
      val chkFlint = check (ChkFlint.checkTop, PPFlint.printFundec, "FLINT")

      val _ = (chkFlint (CGC.checkflint1,1,"1") o prFlint "Translation") flint

      val flint =
	(chkFlint (CGC.checkflint1,1,"2") o prFlint "Lcontract" o lconLexp)
	flint

      val flint =
        if !CGC.specialize then
           (chkFlint (CGC.checkflint1,1,"3") 
            o prFlint "Specialization" o specLexp) flint
        else flint

(*
      (*** explicit FLINT checking phase ***)
      val flint = chkFlint (ref true, 3, "3") flint

      (*** check out the new wrapping function *)
      val nflint1 = (prFlint "NewWrapping" o wrapLexpN) flint
      val nflint2 = chkFlint (ref true, 4, "4") nflint1
      val nflint3 = 
        (chkFlint (ref false, 5, "5") o prFlint "NewReify" o reify) nflint2
      val nlambda = Flint2Lambda.transFundec(nflint3)
      val nlambda =
	(chkLexp (CGC.checklty1,21,"4") o prLexp "NarrowingN" o narrow) nlambda
      val (nfunction,ntable) = convert nlambda 
*)

      val lambda =
	(chkLexp (CGC.checklty1,1,"1")
	 o prLexp "Translation-To-Lambda"
	 o Flint2Lambda.transFundec)
	flint

      val lambda =
	(chkLexp (CGC.checklty1,11,"2") o prLexp "Wrapping" o wrapLexp)
	lambda

      val lambda = (chkLexp (CGC.checklty1,21,"3") o ltyComp) lambda

      val lambda =
	(chkLexp (CGC.checklty1,21,"4") o prLexp "Narrowing" o narrow) lambda

(*
      val lambda = (chkLexp (CGC.checklty2,21,"5") o lambdaopt) lambda
*)

      val (function,table) = convert lambda
      local exception ZZZ
      in val table : FLINT.lty Intmap.intmap = Intmap.new(32, ZZZ) 
      end
      val _ = prCps "convert" function

      val function = (prCps "cpstrans" o cpstrans) function

      val (function,table) = 
        if !CGC.cpsopt then cpsopt (function,table,NONE,false) 
	else (function,table)
      val _ = prCps "cpsopt" function

      fun gen function = let
	  val function = (prCps "closure" o closure) function
	  val carg = globalfix function
	  val carg = spill carg
	  val (carg, limit) = limit carg
      in
	  codegen (carg, limit, err);
	  collect ()
      end

      val fun0 :: funn = CpsSplit.cpsSplit function
      val c0 = gen fun0
      val cn = map gen funn

   in {c0=c0, cn=cn , name=ref (SOME src)}
  end (* function flintcomp *)

val flintcomp = phase "Compiler 050 FLINTComp" flintcomp

end (* local *)
end (* structure FLINTComp *)
