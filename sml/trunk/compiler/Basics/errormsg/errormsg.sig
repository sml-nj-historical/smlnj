(* <errormsg.sig>=                                                          *)
(* Copyright 1989 by AT&T Bell Laboratories *)
signature ERRORMSG =
 sig
    datatype severity = WARN | COMPLAIN
    type complainer (*  = severity -> string -> (PrettyPrintNew.stream -> unit)
                          -> unit *)
    type errorFn = SourceMap.region -> complainer
    type errors (* = {error: errorFn,
                      errorMatch: region->string,
                      anyErrors : bool ref} *)
    val anyErrors : errors -> bool
    exception Error
    val defaultConsumer : unit -> PrettyPrintNew.device
    val nullErrorBody : PrettyPrintNew.stream -> unit
    val error : Source.inputSource -> SourceMap.region -> complainer
    (* with a known location string but without access to the actual source: *)
    val errorNoSource :
	PrettyPrintNew.device * bool ref -> string -> complainer
    val errorNoFile : PrettyPrintNew.device * bool ref -> SourceMap.region
                      -> complainer

    val matchErrorString : Source.inputSource -> SourceMap.region -> string
    val errors : Source.inputSource -> errors
    val errorsNoFile : PrettyPrintNew.device * bool ref -> errors

    val impossible : string -> 'a
    val impossibleWithBody : string -> (PrettyPrintNew.stream -> unit) -> 'a
 end

