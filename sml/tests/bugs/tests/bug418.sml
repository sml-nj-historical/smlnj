(* bug418.sml *)

datatype t = T of int withtype t = string;

T 65:string;
