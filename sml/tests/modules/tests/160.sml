signature TX =
sig

datatype ty = UNDEFty

withtype dtycsig = ty

and dtmembers = dtycsig

end (* signature TYPES *)

structure Tx : TX =
struct

datatype ty = UNDEFty

withtype dtycsig = ty

and dtmembers = dtycsig

end (* structure Types *)

