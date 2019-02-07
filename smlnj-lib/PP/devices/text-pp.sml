(* text-pp.sml
 *
 * COPYRIGHT (c) 2019 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A pretty printer that generates plain text; either to a TextIO.outstream
 * or to a CharBuffer.buf object.  It essentially unifies the behavior of
 * the TextIOPP and CharBufferPP structures.
 *)

structure TextPP : sig

    include PP_STREAM
      where type token = string

    val openOutstream : {dst : TextIO.outstream, wid : int} -> stream

    val openBuffer : {dst : CharBuffer.buf, wid : int} -> stream

  end = struct

    structure Device = struct

	datatype device = DEV of {
	    add1 : char -> unit,
	    addVec : string -> unit,
	    flush : unit -> unit,
	    wid : int
	  }

      (* no style support *)
	type style = unit
	fun sameStyle _ = true
	fun pushStyle _ = ()
	fun popStyle _ = ()
	fun defaultStyle _ = ()

	val openDev = DEV

      (* maximum printing depth (in terms of boxes) *)
	fun depth _ = NONE

      (* the width of the device *)
	fun lineWidth (DEV{wid, ...}) = SOME wid
      (* the suggested maximum width of text on a line *)
	fun textWidth _ = NONE

      (* output some number of spaces to the device *)
	fun space (DEV{addVec, ...}, n) = addVec (StringCvt.padLeft #" " n "")

      (* output a new-line to the device *)
	fun newline (DEV{add1, ...}) = add1 #"\n"

      (* output a string/character in the current style to the device *)
	fun string (DEV{addVec, ...}, s) = addVec s
	fun char (DEV{add1, ...}, c) = add1 c

      (* nothing to flush *)
	fun flush (DEV{flush, ...}) = flush()
      end

    structure PP = PPStreamFn (
      structure Token = StringToken
      structure Device = Device)

    open PP

    fun openOutstream {dst, wid} = openStream(Device.DEV{
	    add1 = fn c => TextIO.output1 (dst, c),
	    addVec = fn c => TextIO.output (dst, c),
	    flush = fn () => TextIO.flushOut dst,
	    wid = wid
	  })

    fun openBuffer {dst, wid} = openStream(Device.DEV{
	    add1 =  fn c => CharBuffer.add1 (dst, c),
	    addVec = fn c => CharBuffer.addVec (dst, c),
	    flush = fn () => (),
	    wid = wid
	  })

  end
