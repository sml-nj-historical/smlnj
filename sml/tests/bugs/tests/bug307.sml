(* bug307.sml *)
(* 0.69: causes uncaught exception Subscript *)

signature SIG1 =
sig
  structure T : sig type t end
  structure U :
      sig
          structure V : sig val s : T.t end
      end
end;

structure S : SIG1 =
struct
  structure T =
  struct
    datatype t = FOO
  end
  structure U =
  struct
    structure V =
    struct
      val s = T.FOO
    end
  end
end;
