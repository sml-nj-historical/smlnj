signature MLTREE_BASIS =
sig

  type attribs = word

  type misc_op = {name:string, hash:word, attribs:attribs ref}

  datatype cond = LT | LTU | LE | LEU | EQ | NE | GE | GEU | GT | GTU 
                | SETCC 
                | MISC_COND of {name:string, hash:word, attribs:word ref}

  datatype fcond = ? | !<=> | == | ?= | !<> | !?>= | < | ?< | !>= | !?> |
                   <= | ?<= | !> | !?<= | > | ?> | !<= | !?< | >= | ?>= |
                   !< | !?= | <> | != | !? | <=> | ?<> | SETFCC |
                  MISC_FCOND of {name:string, hash:word, attribs:word ref}

  datatype rounding_mode = TO_NEAREST | TO_NEGINF | TO_POSINF | TO_ZERO

  datatype ext = SIGN_EXTEND | ZERO_EXTEND

  (* Should be datatypes, but FLINT does not optimize them well *)
  type ty = int
  type fty = int

  (* Invert the conditional when swapping the two arguments
   * of the comparision.  IMPORTANT: this is not the negation!
   *)
  val swapCond : cond -> cond

  (* Invert the conditional when swapping the two arguments
   * of the comparision.  IMPORTANT: this is not the negation!
   *)
  val swapFcond : fcond -> fcond

  (* This is the negation! *)
  val negateCond : cond -> cond

  (* This is the negation! *)
  val negateFcond : fcond -> fcond

  (* hashing functions *)
  val hashCond         : cond -> word
  val hashFcond        : fcond -> word
  val hashRoundingMode : rounding_mode -> word

  (* pretty printing *)
  val condToString         : cond -> string
  val fcondToString        : fcond -> string
  val roundingModeToString : rounding_mode -> string
 
end
