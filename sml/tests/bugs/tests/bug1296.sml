(* bug1296.sml *)

structure S :
  sig
    datatype t = A of b
    and b = B
    structure T :
      sig
	datatype u = datatype t
      end
  end =
struct
  datatype t = A of b
  and b = B
  structure T =
    struct
      datatype u = datatype t
    end
end;
