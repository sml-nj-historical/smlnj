(*
 * COPYRIGHT (c) 1991 by AT&T Bell Laboratories.
 *
 * Various functions for converting strings to
 * integer and real values.
 *
 * AUTHOR:  Emden Gansner
 *	    AT&T Bell Laboratories
 *	    Murray Hill, NJ 07974
 *	    erg@ulysses.att.com
 *)

signature STRING_CVT = sig

  exception BadBase
  exception BadIntRep
  exception BadRealRep

        (* Convert a string representation of an integer to an int.
         * Starts at index first, skips initial white space, accepts
         * optional '~' sign,
         * and quits at first character not recognized in format.
         * Return integer value and index of next unused character.
         * If 1 < base <= 36, strtoi uses that base. 
         * If base = 0, an initial "0" implies octal, 
         * an initial "0x" or "0X" implies hex;
         * otherwise, decimal format is assumed.
         * If base = 16, an initial "0x" or "0X" is allowed.
         *)
  val strtoi : {s : string, first : int, base : int} -> {v : int, next : int}

        (* Convert an integer, in decimal, hex, and octal formats,
         * into an int.
         * Respectively equivalent to:
         *  #v(strtoi{s=s,first=0,base=10}
         *  #v(strtoi{s=s,first=0,base=16}
         *  #v(strtoi{s=s,first=0,base=8}
         *)
  val atoi : string -> int
  val xatoi : string -> int
  val oatoi : string -> int

        (* Convert a string representation of a real to a real.
         * Starts at index first, skips initial white space, accepts
         * optional '~' sign,
         * and quits at first character not recognized in format.
         * Return real value and index of next unused character.
         *)
  val strtor : {s : string, first : int} -> {v : real, next : int} 

        (* Convert a real, in ascii format into a real.
         * Equivalent to:
         *  #v(strtor{s=s,first=0}
         *)
  val atof : string -> real

end (* STRING_CVT *)
