(* run-exene.sml
 *
 * COPYRIGHT (c) 1994 AT&T Bell Laboratories.
 *
 * This structure provides a higher-level interface to invoking applications.
 * Users may set the shell variable "DISPLAY" to specify the display connection.
 *)

structure RunEXene : sig

    val parseDisplay : string -> {
	  host : string, 
	  dpy : string, 
	  screen : string
	}

    val run : (Widget.root -> unit) -> unit

    type options = {
	dpy : string option,		(* specify the display to connect to *)
	timeq : LargeInt.int option	(* specify the CML time quantum in ms. *)
      }

    val runWArgs : (Widget.root -> unit) -> options -> unit

  end = struct

    structure W = Widget
    structure SS = Substring
    structure EXB = EXeneBase

    fun getDpyName NONE = (case (Posix.ProcEnv.getenv "DISPLAY")
	   of NONE => ""
	    | (SOME dpy) => dpy
	  (* end case *))
      | getDpyName (SOME dpy) = dpy

    fun parseDisplay "" = {host="",dpy="0",screen="0"}
      | parseDisplay d = let
          val (host,rest) = SS.splitl (fn c => c <> #":") (SS.all d)
          val (dpy,scr) = SS.splitl (fn c => c <> #".") rest
          in
            if SS.size dpy < 2 then raise EXB.BadAddr "No display field"
            else if SS.size scr = 1 then raise EXB.BadAddr "No screen number"
            else {host=SS.string host,
                  dpy=SS.string(SS.triml 1 dpy),
                  screen=SS.string(SS.triml 1 scr)}
          end

    fun mkRoot dpy = let
          val auth = (case dpy
                 of "" => XAuth.getAuthByAddr {
                        family = XAuth.familyLocal,
                        addr = "",
                        dpy = "0"
                      }
                  | d => let
		      val {dpy,...} = parseDisplay d
		      in
			XAuth.getAuthByAddr {
                            family = XAuth.familyInternet,
                            addr = "",
			    dpy = dpy
			  }
		      end
                 (* end case *))
	  in
            Widget.mkRoot (dpy, auth)
	      handle (EXeneBase.BadAddr s) => (
	        TextIO.output (TextIO.stdErr, String.concat[
		    "eXene: unable to open display \"", dpy, "\"\n",
		    "  ", s, "\n"
		  ]);
	        RunCML.shutdown OS.Process.failure)
	  end

  (* the default time quantum *)
    val defaultTimeQ = Time.fromMilliseconds 20 (* ms *)

    fun run doit = let
	  fun runIt () = let
		val root = mkRoot (getDpyName NONE)
		in
		  doit root
		end
	  in
	    ignore(RunCML.doit (runIt, SOME defaultTimeQ))
	  end

    type options = {
	dpy : string option,		(* specify the display to connect to *)
	timeq : LargeInt.int option	(* specify the CML time quantum in ms. *)
      }

    fun runWArgs doit (opts : options) = let
	  fun runIt () = let
		val root = mkRoot (getDpyName (#dpy opts))
		in
		  doit root
		end
	  val timeQ = (case (#timeq opts)
		 of NONE => defaultTimeQ
		  | (SOME ms) => if (ms <= 0) then defaultTimeQ 
                                 else Time.fromMilliseconds ms
		(* end case *))
	  in
	    ignore (RunCML.doit (runIt, SOME timeQ))
	  end

  end; (* RunEXene *)
