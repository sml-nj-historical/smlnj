(* COPYRIGHT (c) 1998 YALE FLINT PROJECT *)
(* flintcomp.sml *)

functor FLINTComp (structure Gen: MACHINE_GEN
                   val collect: unit -> CodeObj.code_object) : CODEGENERATOR =
struct

local structure CB = CompBasic
      (*        structure CGC = Control.CG *)
      structure MachSpec = Gen.MachSpec
      structure Convert = Convert(MachSpec)
      structure CPStrans = CPStrans(MachSpec)
      structure CPSopt = CPSopt(MachSpec)
      structure Closure = Closure(MachSpec)
      structure Spill = SpillFn(MachSpec)
      structure CpsSplit = CpsSplitFun (MachSpec) 
      structure CTRL = FLINT_Control
      structure PP = PPFlint
      structure LT = LtyExtern
      structure O  = Option
      structure F  = FLINT
in 

structure Machine = Gen
val architecture = Gen.MachSpec.architecture
fun bug s = ErrorMsg.impossible ("FLINTComp:" ^ s)
val say = Control_Print.say
	  
datatype flintkind = FK_WRAP | FK_REIFY | FK_DEBRUIJN | FK_NAMED | FK_CPS
								   
fun phase x = Stats.doPhase (Stats.makePhase x)
	      
val deb2names = phase "Compiler 056 deb2names" TvarCvt.debIndex2names
val names2deb = phase "Compiler 057 names2deb" TvarCvt.names2debIndex
		
val lcontract = phase "Compiler 052 lcontract" LContract.lcontract
(*  val lcontract' = phase "Compiler 052 lcontract'" LContract.lcontract *)
val fcollect  = phase "Compiler 052a fcollect" Collect.collect
val fcontract = phase "Compiler 052b fcontract"
		      (fn (opts,lexp) => FContract.contract opts lexp)
val fcontract = fn opts => fn lexp => fcontract(opts, fcollect lexp)
val loopify   = phase "Compiler 057 loopify" Loopify.loopify
val fixfix    = phase "Compiler 056 fixfix" FixFix.fixfix
val split     = phase "Compiler 058 split" FSplit.split
		
val typelift  = phase "Compiler 0535 typelift" Lift.typeLift
val wformed   = phase "Compiler 0536 wformed" Lift.wellFormed
		
val specialize= phase "Compiler 053 specialize" Specialize.specialize
val wrapping  = phase "Compiler 054 wrapping" Wrapping.wrapping
val reify     = phase "Compiler 055 reify" Reify.reify
val recover   = phase "Compiler 05a recover" Recover.recover

val convert   = phase "Compiler 060 convert" Convert.convert
val cpstrans  = phase "Compiler 065 cpstrans" CPStrans.cpstrans
val cpsopt    = phase "Compiler 070 cpsopt" CPSopt.reduce
val litsplit  = phase "Compiler 075 litsplit" Literals.litsplit
val litToBytes = phase "Compiler 076 litToBytes" Literals.litToBytes
val closure   = phase "Compiler 080 closure"  Closure.closeCPS
val globalfix = phase "Compiler 090 globalfix" GlobalFix.globalfix
val spill     = phase "Compiler 100 spill" Spill.spill
val limit     = phase "Compiler 110 limit" Limit.nolimit
val codegen   = phase "Compiler 120 cpsgen" Gen.codegen

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

      fun check (checkE,printE,chkId) (lvl,logId) e =
	  if checkE (e,lvl) then
	      (dumpTerm (printE, src ^ "." ^ chkId ^ logId, e);
	       bug (chkId ^ " typing errors " ^ logId))
	  else ()
      fun wff (f, s) = if wformed f then ()
		       else print ("\nAfter " ^ s ^ " CODE NOT WELL FORMED\n")

      (* f:prog		flint code
       * fi:prog opt	inlinable approximation of f
       * fk:flintkind	what kind of flint variant this is
       * l:string	last phase through which it went *)
      fun runphase (p,(f,fi,fk,l)) =
	  case (p,fk)
	   of (("fcontract" | "lcontract"), FK_DEBRUIJN) =>
	      (say("\n!! "^p^" cannot be applied to the DeBruijn form !!\n");
	       (f, fi, fk, l))

	    | ("fcontract",_)		=>
	      (fcontract {etaSplit=false, tfnInline=false} f,  fi, fk, p)
	    | ("fcontract+eta",_)	=>
	      (fcontract {etaSplit=true, tfnInline=false} f,  fi, fk, p)
	    | ("lcontract",_)		=> (lcontract f,  fi, fk, p)
	   | ("fixfix",   _)		=> (fixfix f,     fi, fk, p)
	    | ("loopify",  _)		=> (loopify f,    fi, fk, p)
	    | ("specialize",FK_NAMED)	=> (specialize f, fi, fk, p)
	    | ("wrap",FK_NAMED)		=> (wrapping f,	  fi, FK_WRAP, p)
	    | ("reify",FK_WRAP)		=> (reify f,      fi, FK_REIFY, p)
	    | ("deb2names",FK_DEBRUIJN) => (deb2names f,  fi, FK_NAMED, p)
	    | ("names2deb",FK_NAMED)	=> (names2deb f,  fi, FK_DEBRUIJN, p)
	    | ("typelift", _)		=>
	      let val f = typelift f
	      in if !CTRL.check then wff(f, p) else (); (f, fi, fk, p) end
	    | ("split",    FK_NAMED)	=>
	      let val (f,fi) = split f in (f, fi, fk, p) end

	    (* pseudo FLINT phases *)
	    | ("pickle",   _)		=>
	      (valOf(UnpickMod.unpickleFLINT(#pickle(PickMod.pickleFLINT(SOME f)))),
	       UnpickMod.unpickleFLINT(#pickle(PickMod.pickleFLINT fi)),
	       fk, p)
	    | ("collect",_) => (fcollect f, fi, fk, p)
	    | _ =>
	      ((case (p,fk)
		 of ("id",_) => ()
		  | ("wellformed",_) => wff(f,l)
		  | ("recover",_) =>
		    let val {getLty,...} = recover(f, fk = FK_REIFY)
		    in CTRL.recover := (say o LT.lt_print o getLty o F.VAR)
		    end
		  | ("print",_) =>
		    (say("\n[After "^l^"...]\n\n"); PP.printFundec f; say "\n")
		  | ("printsplit", _) => 
		    (say "[ splitted ]\n\n"; O.map PP.printFundec fi; say "\n")
		  | ("check",_) =>
		    (check (ChkFlint.checkTop, PPFlint.printFundec, "FLINT")
			   (fk = FK_REIFY, l) f)
		  | _ =>
		    say("\n!! Unknown or badly scheduled FLINT phase '"^p^"' !!\n"));
		    (f, fi, fk, l))

      fun print (f,fi,fk,l) = (prF l f; (f, fi, fk, l))
      fun check' (f,fi,fk,l) =
	  let fun c n reified f =
		  check (ChkFlint.checkTop, PPFlint.printFundec, n)
			(reified, l) (names2deb f)
	  in if !CTRL.check then
	      (c "FLINT" (fk = FK_REIFY) f; O.map (c "iFLINT" false) fi; ())
	     else ();
		 (f, fi, fk, l)
	  end

      fun showhist [s] = say(concat["  raised at:\t", s, "\n"])
	| showhist (s::r) = (showhist r; say (concat["\t\t", s, "\n"]))
	| showhist [] = ()

      fun runphase' (arg as (p,{1=f,...})) =
	  (if !CTRL.printPhases then say("Phase "^p^"...") else ();
	   ((check' o print o runphase) arg) before
  	   (if !CTRL.printPhases then say("..."^p^" Done.\n") else ()))
	      handle x => (say ("\nwhile in "^p^" phase\n");
			   dumpTerm(PPFlint.printFundec,"flint.core", f);
			   showhist(SMLofNJ.exnHistory x);
			   raise x)

      val (flint,fi,fk,_) = foldl runphase'
				  (flint, NONE, FK_DEBRUIJN, "flintnm")
				  ((* "id" :: *) "deb2names" :: !CTRL.phases)

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
	    val data = litToBytes dlit
            val _ = prC "cpsopt-code" function

(** NOTE: we should be passing the source-code name (src) to the
 ** code generator somehow (for the second argument to code object allocation).
 **)
            fun gen fx = 
              let val fx = (prC "closure" o closure) fx
                  val carg = globalfix fx
                  val carg = spill carg
                  val (carg, limit) = limit carg
               in codegen (carg, limit, err);
                  collect ()
              end

         in case CpsSplit.cpsSplit function
             of (fun0 :: funn) => (gen fun0, map gen funn, data)
              | [] => bug "unexpected case on gen in flintcomp"
        end
   in ({c0=nc0, cn=ncn, data=dseg}, fi)
  end (* function flintcomp *)

val flintcomp = phase "Compiler 050 flintcomp" flintcomp

end (* local *)
end (* structure FLINTComp *)

