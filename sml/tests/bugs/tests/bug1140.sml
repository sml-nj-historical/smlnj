(* bug1140.sml *)

signature BUTTON_VECTOR =
sig
  type key
  structure R : 
    sig
      datatype s = Toggle of key
    end
end 
