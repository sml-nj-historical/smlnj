(* simple tests to make sure that open works correctly *)

signature S0 =
  sig
    structure B : sig type t end
  end

signature S1 =
  sig
    structure A : S0
    open A.B
    val x : t
    type t
    val y : t
    structure C : sig structure D : sig type t end end
    open C
    open D
    val z : t
  end

structure S =
   struct
      structure A = struct structure B = struct type t=int end end
      val x = 5
      type t = bool
      val y = true
      structure C = struct structure D = struct type t=string end end
      val z = "a"
   end

structure S' : S1 = S

	
