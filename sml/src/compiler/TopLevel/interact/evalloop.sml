(* Copyright 1996 by Bell Laboratories *)
(* evalloop.sml *)
 
functor EvalLoopF(Compile: TOP_COMPILE) : EVALLOOP =
struct

local open Environment
      structure C  = Compile
      structure EM = ErrorMsg
      structure E  = Environment
      structure PP = PrettyPrint
      structure T = Time
      structure U = Unsafe
      structure PC = SMLofNJ.Internals.ProfControl
in 

exception Interrupt
type lvar = Access.lvar

val compManagerHook : (Ast.dec * EnvRef.envref -> unit) ref = ref (fn _ => ())

fun installCompManager cm = compManagerHook := cm

val say = Control.Print.say

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

fun evalLoop source = let
    val parser = SmlFile.parseOne source
    val cinfo = C.mkCompInfo { source = source, transform = fn x => x }

    fun checkErrors s = 
        if CompInfo.anyErrors cinfo then raise EM.Error else ()

    fun oneUnit () = (* perform one transaction  *)
	case parser () of
	    NONE => raise EndOfFile
	  | SOME ast => let
		val loc = EnvRef.loc ()
		val base = EnvRef.base ()
		val _ = !compManagerHook (ast, loc)

                val {static=statenv, dynamic=dynenv, symbolic=symenv} =
                    E.layerEnv (#get loc (), #get base ())

                val splitting = Control.LambdaSplitting.get ()
                val {csegments, newstatenv, absyn, exportPid, exportLvars,
                     imports, inlineExp, ...} = 
                    C.compile {source=source, ast=ast,
			       statenv=statenv,
                               symenv=symenv,
			       compInfo=cinfo, 
                               checkErr=checkErrors,
                               splitting=splitting,
			       guid = () }
                (** returning absyn and exportLvars here is a bad idea,
                    they hold on things unnecessarily; this must be 
                    fixed in the long run. (ZHONG)
                 *)

                val executable = Execute.mkexec csegments before checkErrors ()
                val executable = Isolate.isolate (interruptable executable)

                val _ = (PC.current := Profile.otherIndex)
                val newdynenv = 
                    Execute.execute{executable=executable, imports=imports,
				    exportPid=exportPid, dynenv=dynenv}
                val _ = (PC.current := Profile.compileIndex)

                val newenv =
		    E.mkenv { static = newstatenv,
			      dynamic = newdynenv, 
                              symbolic = SymbolicEnv.mk (exportPid,inlineExp) }
                val newLocalEnv = E.concatEnv (newenv, #get loc ())
                    (* refetch loc because execution may 
                       have changed its contents *)

            in
		PrettyPrint.with_pp
		    (#errConsumer source)
		    (fn ppstrm => PPDec.ppDec 
			(E.layerEnv(newLocalEnv, #get base ()))
			ppstrm (absyn, exportLvars));
                    #set loc newLocalEnv
            end

    fun loop() = (oneUnit(); loop())
in
    interruptable loop ()
end (* function evalLoop *)

(*** interactive loop, with error handling ***)
fun interact () = let
    val source = Source.newSource("stdIn",1,TextIO.stdIn,true,
                                  EM.defaultConsumer());
	fun flush'() = 
            case TextIO.canInput(TextIO.stdIn, 4096) of
		NONE => ()
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

	fun exnMsg (CompileExn.Compile s) = concat["Compile: \"", s, "\""]
          | exnMsg (Isolate.TopLevelException e) = exnMsg e
          | exnMsg exn = General.exnMessage exn

	fun showhist (Isolate.TopLevelException e) = showhist e
          | showhist e = showhist' (edit(SMLofNJ.exnHistory e))

	fun loop () = let
	    fun non_bt_hdl e =
		case e of
		    EndOfFile => (say "\n")
                  | Interrupt => (say "\nInterrupt\n"; 
				  flush(); loop())
                  (* | EM.Error => (flush(); loop()) *)
                  | CompileExn.Compile "syntax error" => (flush(); loop())
                  | CompileExn.Compile s =>
                    (say(concat["\nuncaught exception Compile: \"",
				s,"\"\n"]);
                     flush(); loop())
                  | Isolate.TopLevelException Isolate.TopLevelCallcc =>
                    (say("Error: throw from one top-level expression \
			 \into another\n");
                     flush (); loop ())
                  | Isolate.TopLevelException EM.Error =>
                    (flush (); loop ())
                  | Isolate.TopLevelException exn => let
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
		{ work = fn () => evalLoop source,
		  hdl = bt_hdl }
	end
in
    loop()
end (* fun interact *)

fun isTermIn f = 
    let val (rd, buf) = TextIO.StreamIO.getReader(TextIO.getInstream f)
	val isTTY = 
            case rd of
		TextPrimIO.RD{ioDesc = SOME iod, ...} =>
                (OS.IO.kind iod = OS.IO.Kind.tty)
              | _ => false

    (*       val buf = if (buf = "") then NONE else SOME buf *)
    in
	(* since getting the reader will have terminated the stream, we need
         * to build a new stream. *)
	TextIO.setInstream(f, TextIO.StreamIO.mkInstream(rd, buf));
	isTTY
    end

fun evalStream (fname, stream) = let
    val interactive = isTermIn stream
    val source = Source.newSource(fname,1,stream,interactive,
                                  EM.defaultConsumer())
in
    evalLoop source
    handle exn => (Source.closeSource source;
		   case exn of
		       EndOfFile => () 
		     | Isolate.TopLevelException e => raise e
		     | _ => raise exn)
end

end (* top-level local *)
end (* functor EvalLoopF *)
