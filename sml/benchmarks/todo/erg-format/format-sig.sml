(* format-sig.sml
 *
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.
 *
 * Various formatting functions.
 * Provides comparable capabilities to the
 * ANSI C library printf, with similar syntax.
 *
 * AUTHOR:  Emden Gansner
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    erg@ulysses.att.com
 *)

signature FORMAT = sig

  exception BadFormat
  exception ShortList
  exception BadItem

  datatype plus_style_t = Plus | Blank

    (* Format options :
     *  Alternate form, Left adj, Zero pad, Plus style, 
     *  Min. field width, Precision
     *)
  datatype options_t = OPT of {
    altForm : bool,
    leftAdj : bool,
    zeroPad : bool,
    plus : plus_style_t option,
    minWid : int,
    prec : int option
  }

  datatype fitem_t = 
    Int of int 
  | Str of string 
  | Bool of bool 
  | Re of real 
  | Fmt of (options_t -> string)

  (* Type-safe printf emulator. String argument is a format string,
   * In addition to standard printf semantics, it supports
   *   "b", "B" - bool
   *   "&" - user supplied string-valued function
   * and does not support "u", "n", "p" and "c".
   * format returns the generated string.
   * formatf calls the user-supplied function, passing it
   * generated substrings.
   *
   * Exceptions:
   *   BadFormat - Unexpected format character.
   *   ShortList - More format characters than list items.
   *   BadItem   - Type clash between format character and item.
   *)
  val format : string -> fitem_t list -> string
  val formatf : string -> ((string -> 'a) * fitem_t list) -> unit

end (* FORMAT *)
