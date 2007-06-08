(* <errormsg.sml>=                                                          *)
(* Copyright 1989 by AT&T Bell Laboratories *)

structure ErrorMsg : ERRORMSG =
struct

  structure PP = PrettyPrintNew
  open PP SourceMap

 (* error reporting *)

  exception Error  (* was Syntax, changed to Error in 0.92 *)

  datatype severity = WARN | COMPLAIN

  type complainer = severity -> string -> (PP.stream -> unit) -> unit

  type errorFn = region -> complainer

  type errors = {error: region->complainer,
                 errorMatch: region->string,
                 anyErrors: bool ref}

  fun defaultConsumer () = PP.defaultDevice

  val nullErrorBody = (fn (ppstrm: PP.stream) => ())

  fun ppmsg(errConsumer: PP.device, location, severity, msg, body) =
      case (!BasicControl.printWarnings, severity)
	of (false,WARN) => ()
	 | _ =>
	    with_pp errConsumer (fn ppstrm =>
	      (openHVBox ppstrm (PP.Rel 0);
	       openHVBox ppstrm (PP.Rel 2);
	       PP.string ppstrm location;
	       PP.string ppstrm  (* print error label *)
		  (case severity
		     of WARN => " Warning: "
		      | COMPLAIN => " Error: ");
	       PP.string ppstrm msg;
	       body ppstrm;
	       closeBox ppstrm;
	       PP.newline ppstrm;
	       closeBox ppstrm))

  fun record(COMPLAIN,anyErrors) = anyErrors := true
    | record(WARN,_) = ()

  fun impossible msg =
      (app Control_Print.say ["Error: Compiler bug: ",msg,"\n"];
       Control_Print.flush();
       raise Error)
(* With the advent of source-map resynchronization (a.k.a                   *)
(* [[( *#line...* )]]), a contiguous region as seen by the compiler can     *)
(* correspond to one or more contiguous regions in source code.             *)
(* We can imagine myriad ways of displaying such information, but we        *)
(* confine ourselves to two:                                                *)
(* \begin{itemize}                                                          *)
(* \item                                                                    *)
(* When there's just one source region, we have what we had in the old      *)
(* compiler, and we display it the same way:                                *)
(* \begin{quote}                                                            *)
(* {\tt \emph{name}:\emph{line}.\emph{col}} or\\                            *)
(* {\tt \emph{name}:\emph{line1}.\emph{col1}-\emph{line2}.\emph{col2}}      *)
(* \end{quote}                                                              *)
(* \item                                                                    *)
(* When there are two or more source regions, we use an ellipsis instead    *)
(* of a dash, and if not all regions are from the same file, we provide     *)
(* the file names of both endpoints (even if the endpoints are the same     *)
(* file).                                                                   *)
(* \end{itemize}                                                            *)
(*                                                                          *)
(* <errormsg.sml>=                                                          *)
  fun location_string ({sourceMap,fileOpened,...}:Source.inputSource) (p1,p2) =
      let fun shortpoint ({line, column,...}:sourceloc, l) = 
             Int.toString line :: "." :: Int.toString column :: l
          fun showpoint (p as {fileName,...}:sourceloc, l) = 
             Pathnames.trim fileName :: ":" :: shortpoint (p, l)
          fun allfiles(f, (src:sourceloc, _)::l) = 
                f = #fileName src andalso allfiles(f, l)
            | allfiles(f, []) = true
          fun lastpos [(_, hi)] = hi
            | lastpos (h::t) = lastpos t
            | lastpos [] = impossible "lastpos botch in ErrorMsg.location_string"
      in  concat (
            case fileregion sourceMap (p1, p2)
              of [(lo, hi)] => 
                    if p1+1 >= p2 then showpoint (lo, [])
                    else showpoint(lo, "-" :: shortpoint(hi, []))
               | (lo, _) :: rest =>
                    if allfiles(#fileName lo, rest) then
                      showpoint(lo, "..." :: shortpoint(lastpos rest, []))
                    else
                      showpoint(lo, "..." :: showpoint (lastpos rest, []))
               | [] => [Pathnames.trim fileOpened, ":<nullRegion>"]
          )
      end
(* Emulating my predecessors, I've gone to some trouble to avoid list appends (and the *)
(* corresponding allocations).                                              *)
(*                                                                          *)
(* <errormsg.sml>=                                                          *)
  fun error (source as {anyErrors, errConsumer,...}: Source.inputSource)
            (p1:int,p2:int) (severity:severity)
            (msg: string) (body : PP.stream -> unit) = 
      (ppmsg(errConsumer,(location_string source (p1,p2)),severity,msg,body);
       record(severity,anyErrors))

  fun errorNoSource (cons, anyE) locs sev msg body =
      (ppmsg (cons, locs, sev, msg, body); record (sev, anyE))

  fun errorNoFile (errConsumer,anyErrors) ((p1,p2): region) severity msg body = 
      (ppmsg(errConsumer,
             if p2>0 then concat[Int.toString p1, "-", Int.toString p2] 
                     else "",
             severity, msg, body);
       record(severity,anyErrors))

  fun impossibleWithBody msg body =
      (with_pp (defaultConsumer()) (fn ppstrm =>
        (PP.string ppstrm "Error: Compiler bug: ";
         PP.string ppstrm msg;
         body ppstrm;
         PP.newline ppstrm));
       raise Error)

  val matchErrorString = location_string

  fun errors source = 
      {error = error source,
       errorMatch = matchErrorString source,
       anyErrors = #anyErrors source}

  fun anyErrors{anyErrors,error,errorMatch} = !anyErrors

  fun errorsNoFile (consumer,any) =
      {error = errorNoFile (consumer,any),
       errorMatch = fn _ => "Match",
       anyErrors = any}

end  (* structure ErrorMsg *)

