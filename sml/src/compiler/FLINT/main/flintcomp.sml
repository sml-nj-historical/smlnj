(* COPYRIGHT (c) 1998 YALE FLINT PROJECT *)
(* flintcomp.sml *)

functor FLINTComp (structure Gen: CPSGEN
                   val collect: unit -> Word8Vector.vector) : CODEGENERATOR =
struct

local structure CB = CompBasic
(*        structure CGC = Control.CG *)
      structure MachSpec = Gen.MachSpec
      structure Convert = Convert(MachSpec)
      structure CPStrans = CPStrans(MachSpec)
      structure CPSopt = CPSopt(MachSpec)
      structure Closure = Closure(MachSpec)
      structure Spill = Spill(MachSpec)
      structure CpsSplit = CpsSplitFun (MachSpec) 
      structure CTRL = Control.FLINT
in 

val architecture = Gen.MachSpec.architecture
fun bug s = ErrorMsg.impossible ("FLINTComp:" ^ s)
val say = Control.Print.say

datatype flintkind = FK_WRAP | FK_REIFY | FK_DEBRUIJN | FK_NAMED | FK_CPS

fun phase x = Stats.doPhase (Stats.makePhase x)

val lcontract = phase "Compiler 052 lcontract" LContract.lcontract 
val fcollect  = phase "Compiler 052a fcollect" Collect.collect
val fcontract = phase "Compiler 052b fcontract" FContract.contract
val fcontract = fn f => (fcontract(fcollect f, Stats.newCounter[]))
val loopify   = phase "Compiler 057 loopify" Loopify.loopify
val fixfix    = phase "Compiler 056 fixfix" FixFix.fixfix

val typelift  = phase "Compiler 0535 typelift" Lift.typeLift
val wformed   = phase "Compiler 0536 wformed" Lift.wellFormed

val specialize= phase "Compiler 053 specialize" Specialize.specialize
val wrapping  = phase "Compiler 054 wrapping" Wrapping.wrapping
val reify     = phase "Compiler 055 reify" Reify.reify

val deb2names = phase "Compiler 056 deb2names" TvarCvt.debIndex2names
val names2deb = phase "Compiler 057 names2deb" TvarCvt.names2debIndex

val convert   = phase "Compiler 060 convert" Convert.convert
val cpstrans  = phase "Compiler 065 cpstrans" CPStrans.cpstrans
val cpsopt    = phase "Compiler 070 cpsopt" CPSopt.reduce
val litsplit  = phase "Compiler 075 litsplit" Literals.litsplit
val lit2cps   = phase "Compiler 076 lit2cps" Literals.lit2cps
val closure   = phase "Compiler 080 closure"  Closure.closeCPS
val globalfix = phase "Compiler 090 globalfix" GlobalFix.globalfix
val spill     = if MachSpec.spillAreaSz < 500 * MachSpec.valueSize
                then phase "Compiler 100 spill" Spill.spill
                else fn x => x
val limit     = phase "Compiler 110 limit" Limit.nolimit
val codegen   = phase "Compiler 120 cpsgen" Gen.codegen

val closureD  = phase "Compiler 081 closureD"  Closure.closeCPS
val globalfixD= phase "Compiler 091 globalfixD" GlobalFix.globalfix
val spillD    = if MachSpec.spillAreaSz < 500 * MachSpec.valueSize
                then phase "Compiler 101 spillD" Spill.spill
                else fn x => x
val limitD    = phase "Compiler 110 limitD" Limit.nolimit
val codegenD  = phase "Compiler 121 cpsgenD" Gen.codegen

(** pretty printing for the FLINT and CPS code *)
val (prF, prC) = 
  let fun prGen (flag,printE) s e =
        if !flag then (say ("\n[After " ^ s ^ " ...]\n\n"); printE e; 
                       say "\n"; e) 
        else e
   in (prGen (CTRL.print, PPFlint.printProg),
       prGen (Control.CG.printit, PPCps.printcps0))
  end

(** writing out a term into a error output file *)
fun dumpTerm (printE, s, le) =
  let val outS = TextIO.openAppend s;
      val saveOut = !Control.Print.out
      fun done () =
        (TextIO.closeOut outS; Control.Print.out := saveOut)
   in Control.Print.out := {say = fn s => TextIO.output(outS,s),
                            flush = fn () => TextIO.flushOut outS};
      printE le handle x => (done () handle _ => (); raise x);
      done ()
  end (* function dumpTerm *)

val fcs : (FLINT.prog -> FLINT.prog) list ref = ref []

(** compiling FLINT code into the binary machine code *)
fun flintcomp(flint, compInfo as {error, sourceName=src, ...}: CB.compInfo) = 
  let fun err severity s =
 	error (0,0) severity (concat["Real constant out of range: ",s,"\n"])

      fun check (checkE,printE,chkId) (enableChk,lvl,logId) e =
	(if !enableChk andalso checkE (e,lvl) then
	   (dumpTerm (printE, src ^ "." ^ chkId ^ logId, e);
	    bug (chkId ^ " typing errors " ^ logId))
	 else ();
	 e)
      fun chkF (b, s) = 
        check (ChkFlint.checkTop, PPFlint.printFundec, 
               "FLINT") (CTRL.check, b, s)

      fun wff (f, s) = if wformed f then ()
		       else print ("\nAfter " ^ s ^ " CODE NOT WELL FORMED\n")

      (* val fcing = ref (!fcs)
      fun fcontract f =
	  case !fcing
	   of fcontract::fcs => (fcing := fcs; fcontract f)
	    | [] => let val fcc = Stats.newCounter[]
			val fcname = "FContract-"^(Int.toString(length(!fcs)))
			val coname = "FCollect-"^(Int.toString(length(!fcs)))
			val lcname = "LContract-"^(Int.toString(length(!fcs)))
			val fcstat = Stats.newStat(fcname, [fcc])
			val fcphase = phase ("Compiler 052b "^fcname)
					    FContract.contract
			val cophase = phase ("Compiler 052a "^coname)
					    Collect.collect
			val lcphase = phase ("Compiler 052 "^lcname)
					    LContract.lcontract
			fun g c = (lcphase c; fcphase(cophase c,fcc))
	      in
		  Stats.registerStat fcstat;
		  fcs := (!fcs) @ [g];
		  g f
	      end *)
		  
      (* f:FLINT.prog	flint codee
       * r:boot		whether it has gone through reify yet
       * l:string	last phase through which it went *)
      fun runphase (p,(f,fk,l)) =
	  case (p,fk)
	   of (("fcontract" | "lcontract"), FK_DEBRUIJN) =>
	      (say("\n!! "^p^" cannot be applied to the DeBruijn form !!\n");
	       (f, fk, l))

	    | ("fcontract",_)		=> (fcontract f,  fk, p)
	    | ("lcontract",_)		=> (lcontract f,  fk, p)
	    | ("fixfix",   _)		=> (fixfix f,     fk, p)
	    | ("loopify",  _)		=> (loopify f,    fk, p)
	    | ("specialize",FK_NAMED)	=> (specialize f, fk, p)
	    | ("wrap",FK_NAMED)		=> (wrapping f,	  FK_WRAP, p)
	    | ("reify",FK_WRAP)		=> (reify f,      FK_REIFY, p)
	    | ("deb2names",FK_DEBRUIJN) => (deb2names f,  FK_NAMED, p)
	    | ("names2deb",FK_NAMED)	=> (names2deb f,  FK_DEBRUIJN, p)
	    | ("typelift", _)		=>
	      let val f' = typelift f
	      in if !CTRL.check then wff(f', p) else (); (f', fk, p) end

	    (* pseudo FLINT phases *)
	    | ("id",_) => (f,fk,l)
	    | ("collect",_) => (fcollect f, fk, p)
	    | ("print",_) =>
	      (say("\n\n[ After "^l^"... ]\n\n");
	       PPFlint.printFundec f;
	       (f, fk, l) before say "\n")
	    | ("wellformed",_) => (wff(f,l); (f,fk,p))
	    | ("check",_) =>
	      (check (ChkFlint.checkTop, PPFlint.printFundec, "FLINT")
		     (ref true, fk = FK_REIFY, l) f; (f,fk,l))
	    | _ =>
	      (say("\n!! Unknown or badly scheduled FLINT phase '"^p^"' !!\n");
	       (f,fk,l))

      fun print (f,fk,l) = (prF l f; (f, fk, l))
      fun check (f,fk,l) =
	  ((* if fk <> FK_NAMED *) chkF (fk = FK_REIFY, l) (names2deb f) (* else f *);
	   (f, fk, l))

      fun runphase' (arg as (p,{1=f,...})) =
	  (if !CTRL.printPhases then say("Phase "^p^"...") else ();
	   ((check o print o runphase) arg) before
  	   (if !CTRL.printPhases then say("..."^p^" Done.\n") else ()))
	      handle x => (say ("\nwhile in "^p^" phase\n");
			   dumpTerm(PPFlint.printFundec,"FLINT.core", f);
			   raise x)

      (* the "id" phase is just added to do the print/check at the entrance *)
      val (flint,fk,_) = foldl runphase'
			       (deb2names flint, FK_NAMED, "flintnm")
			       ((*  "id" :: *) !CTRL.phases)

      (* run any missing phases *)
      val (flint,fk) =
	  if fk = FK_DEBRUIJN
	  then (say "\n!!Forgot deb2names!!\n"; (deb2names flint, FK_NAMED))
	  else (flint,fk)
      val (flint,fk) =
	  if fk = FK_NAMED
	  then (say "\n!!Forgot wrap!!\n"; (wrapping flint, FK_WRAP))
	  else (flint,fk)
      val (flint,fk) =
	  if fk = FK_WRAP
	  then (say "\n!!Forgot reify!!\n"; (reify flint, FK_REIFY))
	  else (flint,fk)

      (* finish up with CPS *)
      val (nc0, ncn, dseg) = 
        let val function = convert flint
            val _ = prC "convert" function
            val function = (prC "cpstrans" o cpstrans) function
            val function = cpsopt (function,NONE,false) 
            val _ = prC "cpsopt" function

            val (function, dlit) = litsplit function
            val data = lit2cps dlit
            val _ = prC "cpsopt-code" function
            val _ = prC "cpsopt-data" data

            fun gen fx = 
              let val fx = (prC "closure" o closure) fx
                  val carg = globalfix fx
                  val carg = spill carg
                  val (carg, limit) = limit carg
               in codegen (carg, limit, err);
                  collect ()
              end

            fun gdata dd = 
              let val x = Control.CG.printit
                  val y = !x
                  val _ = (x := false)
                  val fx = (prC "closure" o closureD) dd
                  val carg = globalfixD fx
                  val carg = spillD carg
                  val (carg, limit) = limitD carg
               in codegenD (carg, limit, err);
                  (collect ()) before (x := y)
              end
         in case CpsSplit.cpsSplit function
             of (fun0 :: funn) => (gen fun0, map gen funn, gdata data)
              | [] => bug "unexpected case on gen in flintcomp"
        end
   in {c0=nc0, cn=ncn, data=dseg, name=ref (SOME src)}
  end (* function flintcomp *)

val flintcomp = phase "Compiler 050 flintcomp" flintcomp

end (* local *)
end (* structure FLINTComp *)

(*
 * $Log$
 *)
