(* bug1255.sml *)

structure Foo : sig
		  type t
		  datatype u = U of t
		  structure Goo :
		    sig
		      datatype v = datatype u
		    end
		end =
struct
  type t = int
  datatype u = U of t
  structure Goo =
    struct
      datatype v = datatype u
    end
end;
