(* bug1141.sml *)
(* see also bug1125.2.sml *)

functor F () =
struct
  type p = unit
  structure T :
    sig
      type s = p
      datatype t = L of s
    end =
  struct
    type s = unit
    datatype t = L of unit
  end
end;
