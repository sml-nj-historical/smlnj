(* bug112.sml *)

(0.0 = ~0.0, 0.0 = ~ 0.0, ~0.0 = ~ 0.0);    (* (true,true,true) *)

infix eq; fun x eq y = x = y;
(0.0 eq ~0.0, 0.0 eq ~ 0.0, ~0.0 eq ~ 0.0); (* (true,false,false) *)

infix eq; fun (x:real) eq y = x = y;
(0.0 eq ~0.0, 0.0 eq ~ 0.0, ~0.0 eq ~ 0.0); (* (true,true,true) *)
