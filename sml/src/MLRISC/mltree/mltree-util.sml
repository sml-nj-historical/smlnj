(*
 * Basic utility functions for MLTREE
 *
 * -- Allen
 *)

structure MLTreeUtil : MLTREE_UTIL =
struct

  structure Basis = MLTreeBasis
  structure B = MLTreeBasis

  fun swapCond cond =
      case cond of
        B.LT  => B.GT
      | B.LTU => B.GTU
      | B.LE  => B.GE
      | B.LEU => B.GEU
      | B.EQ  => B.EQ
      | B.NE  => B.NE
      | B.GE  => B.LE
      | B.GEU => B.LEU
      | B.GT  => B.LT
      | B.GTU => B.LTU

  fun negateCond cond =
      case cond of
        B.LT  => B.GE
      | B.LTU => B.GEU
      | B.LE  => B.GT
      | B.LEU => B.GTU
      | B.EQ  => B.NE
      | B.NE  => B.EQ
      | B.GE  => B.LT
      | B.GEU => B.LTU
      | B.GT  => B.LE
      | B.GTU => B.LEU

  fun hashCond cond =
      case cond of
        B.LT  => 0w123
      | B.LTU => 0w758
      | B.LE  => 0w81823
      | B.LEU => 0w1231
      | B.EQ  => 0w987
      | B.NE  => 0w8819
      | B.GE  => 0w88123
      | B.GEU => 0w975
      | B.GT  => 0w1287
      | B.GTU => 0w2457

  fun condToString cond =
      case cond of
        B.LT  => "<"
      | B.LTU => "<u"
      | B.LE  => "<="
      | B.LEU => "<=u"
      | B.EQ  => "="
      | B.NE  => "<>"
      | B.GE  => ">="
      | B.GEU => ">=u"
      | B.GT  => ">"
      | B.GTU => ">u"

  fun hashFcond fcond =
      case fcond of
        B.?     => 0w123
      | B.!<=>  => 0w1234
      | B.==    => 0w12345
      | B.?=    => 0w123456
      | B.!<>   => 0w234
      | B.!?>=  => 0w2345
      | B.<     => 0w23456
      | B.?<    => 0w345
      | B.!>=   => 0w3456
      | B.!?>   => 0w34567
      | B.<=    => 0w456
      | B.?<=   => 0w4567
      | B.!>    => 0w45678
      | B.!?<=  => 0w567
      | B.>     => 0w5678
      | B.?>    => 0w56789
      | B.!<=   => 0w678
      | B.!?<   => 0w6789
      | B.>=    => 0w67890
      | B.?>=   => 0w789
      | B.!<    => 0w7890
      | B.!?=   => 0w78901
      | B.<>    => 0w890
      | B.!=    => 0w8901
      | B.!?    => 0w89012
      | B.<=>   => 0w991
      | B.?<>   => 0w391
  
  fun fcondToString fcond =
      case fcond of
        B.?     => "?"
      | B.!<=>  => "!<=>"
      | B.==    => "=="
      | B.?=    => "?="
      | B.!<>   => "!<>"
      | B.!?>=  => "!?>="
      | B.<     => "<"
      | B.?<    => "?<"
      | B.!>=   => "!>="
      | B.!?>   => "!?>"
      | B.<=    => "<="
      | B.?<=   => "?<="
      | B.!>    => "!>"
      | B.!?<=  => "!?<="
      | B.>     => ">"
      | B.?>    => "?>"
      | B.!<=   => "!<="
      | B.!?<   => "!?<"
      | B.>=    => ">="
      | B.?>=   => "?>="
      | B.!<    => "!<"
      | B.!?=   => "!?="
      | B.<>    => "<>"
      | B.!=    => "!="
      | B.!?    => "!?"
      | B.<=>   => "<=>"
      | B.?<>   => "?<>"

  fun hashExt B.SIGN_EXTEND = 0w121287
    | hashExt B.ZERO_EXTEND = 0w71287

  fun extToString B.SIGN_EXTEND = "sext"
    | extToString B.ZERO_EXTEND = "zext"
 
  fun hashRoundingMode m =
      case m of
        B.TO_NEAREST  => 0w1
      | B.TO_NEGINF   => 0w10
      | B.TO_POSINF   => 0w100
      | B.TO_ZERO     => 0w1000

  fun roundingModeToString m =
      case m of
        B.TO_NEAREST  => "to_nearest"
      | B.TO_NEGINF   => "to_neginf"
      | B.TO_POSINF   => "to_posinf"
      | B.TO_ZERO     => "to_zero"

end
