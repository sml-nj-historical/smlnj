(* export.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

structure Export : EXPORT =
  struct

    structure Process = OS_Process

    fun runtimeFn x = CInterface.c_function "SMLNJ-RunT" x

    val exportHeap : string -> bool
	  = runtimeFn "exportHeap"

  (* We need the pair wrapper type to make sure that the second argument will
   * be fully wrapped when it is passed to the run-time system.
   * [also see wrap-export.sml]
   *)
    type cmdt =  (string, string list) WrapExport.pair -> OS.Process.status
    val exportFn' : (string * cmdt) -> unit = runtimeFn "exportFn"

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


(*
 * $Log: export.sml,v $
 * Revision 1.6  1997/10/10 14:13:39  jhr
 *   Added code to raise an exception on null filenames.
 *
 * Revision 1.5  1997/08/13  17:23:12  jhr
 *   Minor clean-up of exportFn code.
 *
 * Revision 1.4  1997/05/05  20:00:00  george
 *   Change the term language into the quasi-A-normal form. Added a new round
 *   of lambda contraction before and after type specialization and
 *   representation analysis. Type specialization including minimum type
 *   derivation is now turned on all the time. Real array is now implemented
 *   as realArray. A more sophisticated partial boxing scheme is added and
 *   used as the default.
 *
 * Revision 1.3  1997/04/10  14:35:18  dbm
 *   Changed return type of exportFn to unit.
 *
 * Revision 1.2  1997/02/11  15:16:14  george
 * moved stuff from System to SMLofNJ
 *
 * Revision 1.1.1.1  1997/01/14  01:38:19  george
 *   Version 109.24
 *
 *)
