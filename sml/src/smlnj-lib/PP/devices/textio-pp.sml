(* textio-pp.sml
 *
 * COPYRIGHT (c) 1999 Bell Labs, Lucent Technologies.
 *
 * A pretty printer with TextIO output; there are no styles and
 * tokens are atoms.
 *)

structure TextIOPP : sig

    include PP_STREAM

    val openOut : {dst : TextIO.outstream, wid : int} -> stream

  end = struct

    structure Tok : PP_TOKEN =
      struct
	type style = unit
	type token = Atom.atom
	val string = Atom.toString
	fun style _ = ()
	fun size s = String.size(Atom.toString s)
      end

    structure PP = PPStreamFn (
      structure Token = Tok
      structure Device = SimpleTextIODev)

    open PP

    fun openOut arg = openStream(SimpleTextIODev.openDev arg)

  end;

