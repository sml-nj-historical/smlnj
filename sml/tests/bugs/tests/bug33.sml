(* bug33.sml *)

signature Sig1 =  (* illegal -- cyclic *)
sig
  structure a: sig structure b: sig end end
  structure a': sig end sharing a = a'
  structure b': sig end sharing b' = a.b
  sharing a' = b'
end;

signature Sig2 =  (* legal *)
sig
  structure a: sig structure b: sig end end
  structure a': sig end sharing a = a'
  structure b': sig end sharing b' = a.b
end;

