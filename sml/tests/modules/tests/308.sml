signature A = 
sig
  type 'a u
  type s = int u  (* 1 *)
  type t = int    (* 2 *)
  sharing type s = t
end
