structure CLocationKinds =
  struct

  (* kinds of locations for passing C arguments *)
    datatype location_kinds
      = K_GPR                (* general-purpose registers *)
      | K_FPR                (* floating-point registers *)
      | K_MEM                (* memory locations *)

  end
