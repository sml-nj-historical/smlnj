(* bug1132.sml *)

signature FOO = 
sig
  type TyVar
  structure TVResult:
    sig
      datatype T = IMP_OK
	         | FAULT of TyVar list
    end
end;
