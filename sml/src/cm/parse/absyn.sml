structure CMAbsyn = struct

    datatype ml_symbol =		(* for now *)
	STRUCTURE of string
      | SIGNATURE of string
      | FUNCTOR of string
      | FUNSIG of string

    type filename = string		(* for now *)

    type requirement = string

    datatype conn_op = ANDALSO | ORELSE | BOOL_EQ | BOOL_NE
    datatype cmp_op = LT | LE | GT | GE | EQ | NE
    datatype arith_op = PLUS | MINUS | TIMES | DIV | MOD

    datatype arithexp =
	NUMBER of int
      | VARIABLE of string
      | ARITH of { arith: arith_op, left: arithexp, right: arithexp }
      | NEGATE of arithexp

    datatype boolexp =
	ML_DEFINED of ml_symbol
      | CM_DEFINED of string
      | LCONN of { conn: conn_op, left: boolexp, right: boolexp }
      | NOT of boolexp
      | CMP of { cmp: cmp_op, left: arithexp, right: arithexp }

    datatype 'a guardian =
	GUARDIAN of { test: boolexp,
		      then_part: 'a,
		      else_part: 'a }

    datatype export =
	EXPORT of ml_symbol
      | GUARDED_EXPORT of export list guardian

    datatype member =
	MEMBER of { name: filename, class: string option }
      | GUARDED_MEMBER of member list guardian

    type 'a groupspec =
	{ adds: requirement list,
	  meets: requirement list,
	  exports: 'a,
	  members: member list }

    datatype description =
	ALIAS of filename
      | GROUP of export list option groupspec
      | LIBRARY of export list groupspec
end
