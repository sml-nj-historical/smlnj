(* char-buffer-pp.sml
 *
 * COPYRIGHT (c) 2018 The Fellowship of SML/NJ (http://www.smlnj.org)
 * All rights reserved.
 *
 * A pretty printer that puts its output in a CharBuffer.buf object.  There
 * are no styles and tokens are strings.  You can use this module to pretty-print
 * into a string as follows:
 *
 *	val buf = CharBuffer.new 1024
 *	val ppStrm = CharBufferPP.openBuf {dst = buf, wid = 80}
 *	.... pretty printing ....
 *	val result = CharBuffer.contents buf
 *)

structure CharBufferPP : sig

    include PP_STREAM
      where type token = string

    val openBuf : {dst : CharBuffer.buf, wid : int} -> stream

  end = struct

    structure Device = struct

	datatype device = DEV of {
	    dst : CharBuffer.buf,
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
	fun space (DEV{dst, ...}, n) = CharBuffer.addVec (dst, StringCvt.padLeft #" " n "")

      (* output a new-line to the device *)
	fun newline (DEV{dst, ...}) = CharBuffer.add1 (dst, #"\n")

      (* output a string/character in the current style to the device *)
	fun string (DEV{dst, ...}, s) = CharBuffer.addVec (dst, s)
	fun char (DEV{dst, ...}, c) = CharBuffer.add1 (dst, c)

      (* nothing to flush *)
	fun flush _ = ()
      end

    structure PP = PPStreamFn (
      structure Token = StringToken
      structure Device = Device)

    open PP

    fun openBuf arg = openStream(Device.openDev arg)

  end;

