(* bug817.sml *)
(* 817. spec scope confusion *)

Compiler.Control.Print.signatures := 20;

signature prints_wrong =
sig
  structure y :
    sig
      type b
      val x : b
    end
end;

signature illegal_sig =
sig
  structure y :
    sig
      type b
      val x : y.b
    end
end;

signature wrong_sig =
sig
  structure y : sig type b end
  structure z : sig
	     	  structure y :
		    sig
		      type b
		      val x : y.b
		    end
		end
end;
