(* space.sml
 *
 * COPYRIGHT (c) 1993, AT&T Bell Laboratories.
 *
 * The quad/oct-tree representation of space.
 *)

signature SPACE =
  sig

    structure V : VECTOR

    datatype body = Body of {
	proc : int ref,
	mass : real,
	pos : real V.vec ref,
	vel : real V.vec ref,
	acc : real V.vec ref,
	phi : real ref
      }

    datatype cell
      = BodyCell of body
      | Cell of node Array.array

    and node
      = Empty
      | Node of {
	  proc : int ref,
	  mass : real ref,
	  pos : real V.vec ref,
	  cell : cell
	}

    datatype space = Space of {
	rmin : real V.vec,
	rsize : real,
	root : node
      }

    val nsub : int	(* number of sub cells / cell (2 ^ V.dim) *)

    val putCell : (cell * int * node) -> unit
    val getCell : (cell * int) -> node
    val mkCell : unit -> cell
    val mkBodyNode : body -> node
    val mkCellNode : cell -> node

  (* debugging code *)
    val dumpTree : node -> unit
    val prBody : body -> string
    val prNode : node -> string

  end; (* SPACE *)

functor Space (V : VECTOR) : SPACE =
  struct

    structure V = V

    datatype body = Body of {
	proc : int ref,
	mass : real,
	pos : real V.vec ref,
	vel : real V.vec ref,
	acc : real V.vec ref,
	phi : real ref
      }

    datatype cell
      = BodyCell of body
      | Cell of node Array.array

    and node
      = Empty
      | Node of {
	  proc : int ref,
	  mass : real ref,
	  pos : real V.vec ref,
	  cell : cell
	}

    datatype space = Space of {
	rmin : real V.vec,
	rsize : real,
	root : node
      }

  (* number of sub cells per cell (2 ^ V.dim) *)
    val nsub = Bits.lshift(1, V.dim)

    fun putCell (Cell a, i, nd) = Array.update(a, i, nd)
    fun getCell (Cell a, i) = Array.sub(a, i)
    fun mkCell () = Cell(Array.array(nsub, Empty))
    fun mkBodyNode (body as Body{proc, pos, mass, ...}) = Node{
	    proc = proc,
	    cell = BodyCell body,
	    mass = ref mass,
	    pos = ref (!pos)
	  }
    fun mkCellNode cell = Node{
	    proc = ref 0,
	    cell = cell, mass = ref 0.0, pos = ref V.zerov
	  }

  (* debugging code *)
    local
      fun cvtFn r = Format.format "%f" [Format.REAL r]
      val vfmt = V.format{lp="[", rp="]", sep=",", cvt = cvtFn}
      fun vecFmt vec = Format.STR(vfmt vec)
      val bodyFmt = Format.format "B{m=%f, p=%s, v=%s, a=%s, phi=%f}"
      val cellFmt = Format.format "N{m=%f, p=%s, %s}"
    in
    fun prBody (Body{proc, mass, pos, vel, acc, phi}) = bodyFmt [
	    Format.REAL mass, vecFmt(!pos), vecFmt(!vel), vecFmt(!acc),
	    Format.REAL(!phi)
	  ]
    fun prNode Empty = "Empty"
      | prNode (Node{proc, mass, pos, cell}) = let
	  val cell = (case cell
		 of (Cell _) => "Cell"
		  | (BodyCell b) => (*prBody b*) "Body"
		(* end case *))
	  in
	    cellFmt [Format.REAL(!mass), vecFmt(!pos), Format.STR cell]
	  end
    end

    fun dumpTree tree = let
	  fun printf (fmt, items) = IO.output(IO.std_out, Format.format fmt items)
	  fun indent i = Makestring.padLeft("", i+i)
	  fun dump (node, l) = let
		fun dump' (Node{cell=Cell a, ...}) = let
		      fun dump'' i = (dump(Array.sub(a, i), l+1); dump''(i+1))
		      in
			(dump'' 0) handle _ => ()
		      end
		  | dump' _ = ()
		in
		  printf("%2d: %s%s\n", [
		      Format.INT l, Format.STR(indent l), Format.STR(prNode node)
		    ]);
		  dump' node
		end
	  in
	    dump (tree, 0)
	  end

  end; (* Space *)

