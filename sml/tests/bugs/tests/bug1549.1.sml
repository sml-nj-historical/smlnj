(* bug1549.1.sml *)

signature NUMBER =
sig
  type t
end;

signature VEC =
sig
  structure Number : NUMBER
  type t
  val fromSeq : Number.t Vector.vector -> t
  val toSeq   : t -> Number.t Vector.vector
end;

functor PointFromVector(structure Vec : VEC) =
struct
  structure Vec = Vec
end;

signature GEOMETRY_PRIMS =
sig
  structure Point : sig
		      structure Vec : VEC
		    end
end;

functor GeometryPrims2d (structure Number : NUMBER)
    : GEOMETRY_PRIMS =
struct
  structure Vec =
  struct
    structure Number = Number
    type t = Number.t * Number.t
    fun fromSeq #[x, y] : t = (x, y)
    fun toSeq (x,y) = #[x, y]
  end

  structure Point = PointFromVector(structure Vec = Vec)
end;

structure RealNumber =
struct
  type t = real
end;

structure RealGeometryPrims2d = GeometryPrims2d(structure Number = RealNumber);

local
  (* #[6.0,0.0] avoids errors *)
  val vs = RealGeometryPrims2d.Point.Vec.toSeq (6.0, 0.0)
in
  val foo = Array.tabulate (Vector.length vs,
			    fn i => Vector.sub (vs, i))
end;
