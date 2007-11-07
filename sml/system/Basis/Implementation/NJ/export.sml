(* export.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Export : EXPORT =
  struct

    structure Process = OS_Process

    val exportHeap : string -> bool = SMLNJRuntime.exportHeap

  (* We need the pair wrapper type to make sure that the second argument will
   * be fully wrapped when it is passed to the run-time system.
   * [also see wrap-export.sml]
   *)
    type cmdt =  (string, string list) WrapExport.pair -> OS.Process.status
(*    val exportFn' : (string * cmdt) -> unit = SMLNJRuntime.exportFn *)
fun exportFn' _ = raise Fail "todo: implement exportFn'"

    fun nullFilename () = raise Assembly.SysErr("empty heap file name", NONE)

  (* export the current world to the given file *)
    fun exportML "" = nullFilename()
      | exportML fileName = (
	  CleanUp.clean CleanUp.AtExportML;
	  if (exportHeap fileName)
	    then (
	      CleanUp.clean CleanUp.AtInit;
	      true)
	  else false)

    fun exportFn ("", f) = nullFilename()
      | exportFn (fileName, f) = (
	  Signals.maskSignals Signals.MASKALL;
	  CleanUp.clean CleanUp.AtExportFn;
	  Assembly.pstruct := InlineT.cast ();
	  exportFn' (fileName, WrapExport.wrap f)
	  (* this never returns here *))

  end (* Export *)


