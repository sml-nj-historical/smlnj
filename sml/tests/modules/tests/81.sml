(* test error messages for an unbound symbol *)

structure A =
  struct
       type t = a
  end

(* test ability to keep going *)

structure B =
  struct
      type t = A.t
  end

(* test error message for an unbound structure in a symbolic path *)

structure C =
  struct
      type x = A.D.t
  end

(* test error message for an unbound first structure in a symbolic path *)

structure D =
  struct
      type x = Bogus.t
  end
