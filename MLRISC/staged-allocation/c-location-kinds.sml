structure CLocationKinds =
  struct

  (* kinds of locations for passing C arguments *)
    datatype location_kind
      = K_GPR                (* general-purpose registers *)
      | K_FPR                (* floating-point registers *)
      | K_MEM                (* memory locations *)
      | K_FMEM               (* floating-point memory locations *)

  end
