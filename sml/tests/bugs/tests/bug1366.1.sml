(* bug1366.1.sml *)

structure Opaque :>
  sig
    type sort
    val base : sort
    functor Next(val s : sort) : sig val t : sort end
  end =
struct
  type sort = int
  val base = 0
  functor Next(val s : sort) =
    struct
      val t = s + 1
    end
end
