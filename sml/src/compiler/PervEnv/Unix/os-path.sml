(* os-path.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * This is the UNIX implementation of the generic OS.Path structure.
 *
 *)

structure OS_Path = OS_PathFn (
  struct

    exception Path

    datatype arc_kind = Null | Parent | Current | Arc of string

    fun classify "" = Null
      | classify "." = Current
      | classify ".." = Parent
      | classify a = Arc a

    val parentArc = ".."

    val currentArc = "."

    fun validVolume (_, vol)= Substring.isEmpty vol

    val volSS = Substring.all ""

  (* Note: we are guaranteed that this is never called with "" *)
    fun splitVolPath s = if (InlineT.CharVector.sub(s, 0) = #"/")
	  then (true, volSS, Substring.triml 1 (Substring.all s))
	  else (false, volSS, Substring.all s)

    fun joinVolPath (true, "", "") = "/"
      | joinVolPath (true, "", s) = "/" ^ s
      | joinVolPath (false, "", s) = s
      | joinVolPath _ = raise Path (* invalid volume *)

    val arcSepChar = #"/"

  end);

(*
 * $Log: os-path.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:58  george
 * Version 110.5
 *
 *)
