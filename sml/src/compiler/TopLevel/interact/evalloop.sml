(* Copyright 1996 by Bell Laboratories *)
(* evalloop.sml *)
 
functor EvalLoopF(Compile: TOP_COMPILE) : EVALLOOP =
struct

local open Environment
      structure C  = Compile
      structure CB = CompBasic
      structure EM = ErrorMsg
      structure E  = Environment
      structure PP = PrettyPrint
      structure T = Time
      structure U = Unsafe
      structure PC = SMLofNJ.Internals.ProfControl
in 

exception Interrupt
type lvar = Access.lvar

type interactParams = 
   {compManagerHook :
    (CB.ast * EnvRef.envref * EnvRef.envref -> unit) option ref,
          baseEnvRef      : EnvRef.envref,
          localEnvRef     : EnvRef.envref,
          transform       : CB.absyn -> CB.absyn,
          instrument      : {source: CB.source,
                             compenv: StaticEnv.staticEnv}
                                -> (CB.absyn -> CB.absyn),
          perform         : CB.executable -> CB.executable,
          isolate         : CB.executable -> CB.executable,
          printer         : E.environment -> PP.ppstream 
                            -> (CB.absyn * lvar list) -> unit}

val stdParams : interactParams =
      {compManagerHook = ref NONE,
       baseEnvRef = EnvRef.pervasive,
       localEnvRef = EnvRef.topLevel,
       transform = (fn x => x),
       instrument = (fn _ => fn x => x),
       perform = (fn x => x),
       isolate = C.isolate,
       printer = PPDec.ppDec}

(* toplevel loop *)

val say = Control.Print.say
fun debugmsg msg = 
  if !Control.debugging	then (say (msg ^ "\n"); Control.Print.flush()) else ()

exception EndOfFile

fun interruptable f x =
    let val oldcont = !U.topLevelCont
     in U.topLevelCont :=
         SMLofNJ.Cont.callcc
            (fn k => (SMLofNJ.Cont.callcc(fn k' => (SMLofNJ.Cont.throw k k'));
	              raise Interrupt));
        (f x before U.topLevelCont := oldcont)
        handle e => (U.topLevelCont := oldcont; raise e)
    end

(* 
 * The baseEnv and localEnv are purposely refs so that a top-level command
 * can re-assign either one of them, and the next iteration of the loop
 * will see the new value. It's also important that the toplevelenv
 * continuation NOT see the "fetched" environment, but only the ref;
 * then, if the user "filters" the environment ref, a smaller image
 * can be written. 
 *)

fun evalLoop ({compManagerHook, baseEnvRef, localEnvRef, perform, 
               isolate, printer, instrument, transform} : interactParams)
             (source: Source.inputSource) : unit =

let val parser = C.parseOne source
    val cinfo = C.mkCompInfo(source,transform)

    fun checkErrors s = 
        if C.anyErrors cinfo then raise EM.Error else ()

    fun oneUnit () = (* perform one transaction  *)
      case parser ()
       of NONE => raise EndOfFile
	| SOME ast =>
	    let val _ = case !compManagerHook
                 of NONE => ()
                  | SOME cm => cm (ast, baseEnvRef, localEnvRef)

                val {static=statenv, dynamic=dynenv, symbolic=symenv} =
                  E.layerEnv(#get localEnvRef (), #get baseEnvRef ())

                val splitting = Control.LambdaSplitting.get ()
                val {csegments, newstatenv, absyn, exportPid, exportLvars,
                     imports, inlineExp, ...} = 
                  C.compile {source=source, ast=ast,
			     statenv=statenv,
                             symenv=symenv,
			     compInfo=cinfo, 
                             checkErr=checkErrors,
                             splitting=splitting}
                (** returning absyn and exportLvars here is a bad idea,
                    they hold on things unnecessarily; this must be 
                    fixed in the long run. (ZHONG)
                 *)

                val executable = C.mkexec csegments before checkErrors ()
                val executable = isolate (interruptable (perform executable))

                val _ = (PC.current := Profile.otherIndex)
                val newdynenv = 
                  C.execute{executable=executable, imports=imports,
                            exportPid=exportPid, dynenv=dynenv}
                val _ = (PC.current := Profile.compileIndex)

                val newenv = E.mkenv {static=newstatenv, dynamic=newdynenv, 
                                      symbolic=C.mksymenv(exportPid,inlineExp)}
                val newLocalEnv = E.concatEnv(newenv, #get localEnvRef ())
                    (* refetch localEnvRef because execution may 
                       have changed its contents *)

             in PrettyPrint.with_pp (#errConsumer source)
                (fn ppstrm => printer 
                  (E.layerEnv(newLocalEnv, #get baseEnvRef ()))
                  ppstrm (absyn, exportLvars));
                #set localEnvRef newLocalEnv
            end

    fun loop() = (oneUnit(); loop())
 in interruptable loop ()
end (* function evalLoop *)

(*** interactive loop, with error handling ***)
fun interact (interactParams) : unit =
    let val source = Source.newSource("stdIn",1,TextIO.stdIn,true,
                                   EM.defaultConsumer());
       fun flush'() = 
           case TextIO.canInput(TextIO.stdIn, 4096)
             of NONE => ()
              | (SOME 0) => ()
              | (SOME _) => (ignore (TextIO.input TextIO.stdIn); flush'())

       fun flush() = (#anyErrors source := false; 
                     flush'() handle IO.Io _ => ())

        local val p1 = Substring.isPrefix "TopLevel/interact/evalloop.sml:"
              val p2 = Substring.isPrefix "TopLevel/main/compile.sml:"
              val p3 = Substring.isPrefix "MiscUtil/util/stats.sml:"
           in fun edit [s] = [s]
                | edit nil = nil
                | edit (s::r) = 
                   let val s' = Substring.all s
                    in if p1 s' orelse p2 s' orelse p3 s'
                       then edit r else s :: edit r
                   end
          end

       fun showhist' [s] = say(concat["  raised at: ", s, "\n"])
         | showhist' (s::r) = (showhist' r; 
                            say (concat["             ", s, "\n"]))
         | showhist' [] = ()

       fun exnMsg (Compile.Compile s) = concat["Compile: \"", s, "\""]
         | exnMsg (C.TopLevelException e) = exnMsg e
         | exnMsg exn = General.exnMessage exn

       fun showhist (C.TopLevelException e) = showhist e
         | showhist C.SilentException = ()
         | showhist e = showhist' (edit(SMLofNJ.exnHistory e))

       fun loop () = let
	   fun non_bt_hdl e =
	       case e of
		   EndOfFile => (say "\n")
                 | Interrupt => (say "\nInterrupt\n"; 
				 flush(); loop())
                 (* | EM.Error => (flush(); loop()) *)
                 | C.Compile "syntax error" => (flush(); loop())
                 | C.Compile s =>
                   (say(concat["\nuncaught exception Compile: \"",
			       s,"\"\n"]);
                    flush(); loop())
                 | C.TopLevelException C.TopLevelCallcc =>
                   (say("Error: throw from one top-level expression \
			\into another\n");
                    flush (); loop ())
                 | C.TopLevelException EM.Error =>
                   (flush (); loop ())
                 | C.TopLevelException C.SilentException =>
                   (flush (); loop ())
                 | C.TopLevelException exn => let
		       val msg = exnMsg exn
		       val name = exnName exn
                   in
		       if (msg = name)
		       then say (concat
				     ["\nuncaught exception ",
				      exnName exn, "\n"])
		       else say (concat
				     ["\nuncaught exception ", exnName exn,
				      " [", exnMsg exn, "]\n"]);
		       showhist exn;
		       flush(); 
		       loop()
                   end
                 | C.SilentException => (flush (); loop ())
                 | exn => (say (concat["\nuncaught exception ", 
				       exnMsg exn, "\n"]);
			   showhist exn;
			   flush();
			   loop())
	   fun bt_hdl (e, []) = non_bt_hdl e
	     | bt_hdl (e, hist) =
	       (say (concat ("\n*** BACK-TRACE ***\n" :: hist));
		say "\n";
		non_bt_hdl e)
       in
	   SMLofNJ.Internals.BTrace.bthandle
	       { work = fn () => evalLoop interactParams source,
		 hdl = bt_hdl }
       end
    in loop()
    end (* fun interact *)

fun isTermIn f = 
    let val (rd, buf) = TextIO.StreamIO.getReader(TextIO.getInstream f)
       val isTTY = 
           case rd
             of TextPrimIO.RD{ioDesc = SOME iod, ...} =>
                 (OS.IO.kind iod = OS.IO.Kind.tty)
              | _ => false

(*       val buf = if (buf = "") then NONE else SOME buf *)
     in (* since getting the reader will have terminated the stream, we need
        * to build a new stream. *)
       TextIO.setInstream(f, TextIO.StreamIO.mkInstream(rd, buf));
       isTTY
    end

fun evalStream interactParams 
               (fname:string,stream:TextIO.instream) : unit =
    let val interactive = isTermIn stream
       val source = Source.newSource(fname,1,stream,interactive,
                                  EM.defaultConsumer())
     in evalLoop interactParams source
       handle exn =>
         (Source.closeSource source;
          case exn 
          of EndOfFile => () 
           | C.TopLevelException e => raise e
           | _ => raise exn)
    end 

end (* top-level local *)
end (* functor EvalLoopF *)

