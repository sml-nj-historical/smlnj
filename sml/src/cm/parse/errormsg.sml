signature ERRORMSG = sig
    val anyErrors : unit -> bool
    val newLine : int -> unit
    val lastLinePos : unit -> int
    val error : int -> string -> unit
    exception Error
    val impossible : string -> 'a   (* raises Error *)
    val init : string -> unit
end

structure ErrorMsg :> ERRORMSG = struct

  val anyErrors = ref false
  val fileName = ref ""
  val lineNum = ref 1
  val linePos = ref [1]

  fun newLine p = (lineNum := !lineNum + 1; linePos := p :: !linePos)
  fun lastLinePos () = hd (!linePos)

  fun init fname = (anyErrors := false;
		    fileName := fname;
		    lineNum := 1;
		    linePos := [1])

  exception Error

  fun error pos (msg:string) = let
      fun look (a :: rest, n) =
	  if a < pos then
	      app print [":", Int.toString n, ".", Int.toString (pos - a)]
	  else look(rest, n - 1)
	| look _ = print "0.0"
  in
      anyErrors := true;
      print (!fileName);
      look (!linePos, !lineNum);
      print ":";
      print msg;
      print "\n"
  end

  fun impossible msg =
      (app print ["Error: Compiler bug: ",msg,"\n"];
       TextIO.flushOut TextIO.stdOut;
       raise Error)

  val anyErrors = fn () => !anyErrors

end

