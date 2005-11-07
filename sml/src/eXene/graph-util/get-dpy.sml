(* get-dpy.sml
 *
 * COPYRIGHT (c) 1998 Bell Labs, Lucent Technologies.
 *
 * Utility code for getting the display name and authentication information.
 *)

structure GetDpy : GET_DPY =
  struct

    structure EXB = EXeneBase
    structure SS = Substring

    fun getDpyName NONE = (case (OS.Process.getEnv "DISPLAY")
	   of NONE => ""
	    | (SOME dpy) => dpy
	  (* end case *))
      | getDpyName (SOME dpy) = dpy

  (* parse a string specifying a X display into its components. *)
    fun parseDisplay "" = {host="",dpy="0",screen="0"}
      | parseDisplay d = let
          val (host,rest) = SS.splitl (fn c => c <> #":") (SS.full d)
          val (dpy,scr) = SS.splitl (fn c => c <> #".") rest
          in
            if SS.size dpy < 2 then raise EXB.BadAddr "No display field"
            else if SS.size scr = 1 then raise EXB.BadAddr "No screen number"
            else {host=SS.string host,
                  dpy=SS.string(SS.triml 1 dpy),
                  screen=SS.string(SS.triml 1 scr)}
          end

  (* given an optional display name, return the display and authentication
   * information.  If the argument is NONE, then we use the DISPLAY environment
   * variable if it is defined, and "" if it is not defined.
   *)
    fun getDpy dpyOpt = let
	  val dpy = getDpyName dpyOpt
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
	    (dpy, auth)
	  end

  end;

