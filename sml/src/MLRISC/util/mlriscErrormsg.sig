signature MLRISC_ERROR_MSG = sig
  exception Error
  val print : string -> unit
  val impossible : string -> 'a
end