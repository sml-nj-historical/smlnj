structure MLTreeBasis : MLTREE_BASIS =
struct
 
  datatype cond = LT | LTU | LE | LEU | EQ | NE | GE | GEU | GT | GTU 

  datatype fcond = ? | !<=> | == | ?= | !<> | !?>= | < | ?< | !>= | !?> |
                   <= | ?<= | !> | !?<= | > | ?> | !<= | !?< | >= | ?>= |
                   !< | !?= | <> | != | !? | <=> | ?<>

  datatype ext = SIGN_EXTEND | ZERO_EXTEND

  datatype rounding_mode = TO_NEAREST | TO_NEGINF | TO_POSINF | TO_ZERO

  type attribs = word

  type misc_op = {name:string,attribs:attribs,hash:word}

  nonfix <> < > >= <=

  (* These should be datatypes, but FLINT does not optimize them well *)
  type ty = int
  type fty = int

  fun swapCond cond =
      case cond of
        LT  => GT | LTU => GTU | LE  => GE | LEU => GEU | EQ  => EQ 
      | NE  => NE | GE  => LE | GEU => LEU | GT  => LT | GTU => LTU

  fun negateCond cond =
      case cond of
        LT  => GE | LTU => GEU | LE  => GT | LEU => GTU | EQ  => NE
      | NE  => EQ | GE  => LT | GEU => LTU | GT  => LE | GTU => LEU

  fun hashCond cond =
      case cond of
        LT  => 0w123 | LTU => 0w758 | LE  => 0w81823 | LEU => 0w1231
      | EQ  => 0w987 | NE  => 0w8819 | GE  => 0w88123 | GEU => 0w975
      | GT  => 0w1287 | GTU => 0w2457

  fun condToString cond =
      case cond of
        LT  => "<" | LTU => "<u" | LE  => "<=" | LEU => "<=u"
      | EQ  => "=" | NE  => "<>" | GE  => ">=" | GEU => ">=u"
      | GT  => ">" | GTU => ">u"

  fun hashFcond fcond =
      case fcond of
        ?     => 0w123 | !<=>  => 0w1234 | ==    => 0w12345 | ?=    => 0w123456
      | !<>   => 0w234 | !?>=  => 0w2345 | <   => 0w23456 | ?<    => 0w345
      | !>=   => 0w3456 | !?>   => 0w34567 | <=  => 0w456   | ?<=   => 0w4567
      | !>    => 0w45678 | !?<=  => 0w567 | >  => 0w5678  | ?>    => 0w56789
      | !<=   => 0w678 | !?<   => 0w6789 | >=    => 0w67890 | ?>=   => 0w789
      | !<    => 0w7890 | !?=   => 0w78901 | <>    => 0w890 | !=    => 0w8901
      | !?    => 0w89012 | <=>   => 0w991 | ?<>   => 0w391
  
  fun fcondToString fcond =
      case fcond of
        ?     => "?" | !<=>  => "!<=>" | ==    => "==" | ?=    => "?="
      | !<>   => "!<>" | !?>=  => "!?>=" | <     => "<" | ?<    => "?<"
      | !>=   => "!>=" | !?>   => "!?>" | <=    => "<=" | ?<=   => "?<="
      | !>    => "!>" | !?<=  => "!?<=" | >     => ">" | ?>    => "?>"
      | !<=   => "!<=" | !?<   => "!?<" | >=    => ">=" | ?>=   => "?>="
      | !<    => "!<" | !?=   => "!?=" | <>    => "<>" | !=    => "!="
      | !?    => "!?" | <=>   => "<=>" | ?<>   => "?<>"

  fun hashRoundingMode m =
      case m of
        TO_NEAREST => 0w1 | TO_NEGINF => 0w10 
      | TO_POSINF => 0w100 | TO_ZERO     => 0w1000

  fun roundingModeToString m =
      case m of
        TO_NEAREST  => "to_nearest" | TO_NEGINF   => "to_neginf"
      | TO_POSINF   => "to_posinf" | TO_ZERO     => "to_zero"

end (* MLTreeBasis *)
