(* bug114.sml -- include doesn't work *)

signature old = sig type t end;

signature new = sig include old end;
