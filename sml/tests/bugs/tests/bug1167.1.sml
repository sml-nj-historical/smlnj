(* bug1167.1.sml *)

type atom = string
type text = string
val minprec = ~1
val maxprec = 101
type precedence = int
datatype associativity = LEFT | RIGHT | NONASSOC
datatype fixity = PREFIX | POSTFIX | INFIX of associativity
type rator = text * precedence * fixity
fun is_prefix (_, _, f) = f = PREFIX
val juxtarator = ("", maxprec - 1, INFIX LEFT)  (* SML function application *)
datatype exp = ATOMexp of atom
             | UNARY   of rator * exp
             | BINARY  of exp * rator * exp
datatype image  = IMAGE of lexeme list
and      lexeme = ATOMIC of atomic
                | RATOR  of rator
and      atomic = ATOMimage of atom
                | PARENS of image
fun noparens(inner as (_, pi, fi) : rator, outer as (_, po, fo), side) =
  pi > po orelse
  case (side, fi)
    of (LEFT,  POSTFIX)     => true
     | (LEFT,  INFIX LEFT)  => fo = INFIX LEFT
     | (RIGHT, POSTFIX)     => true
     | (RIGHT, INFIX RIGHT) => fo = INFIX RIGHT
     | (NONASSOC, _) => fi = fo
     | _ => false
type infixop = rator
type prefix = rator
datatype stack = BOT
               | BIN of infixop * exp * prefix list * stack
type stacktop = (prefix list * exp)
val minrator = ("<phony minimum-precedence operator>", minprec, INFIX NONASSOC)
fun srator  (BIN ($, _, _, _)) = $
  | srator  BOT = minrator
exception ParseError of string * rator list
local
  exception Impossible
  fun parse_prefix(stack, prefixes, ATOMIC a :: ipts') = 
	parse_postfix(stack, (exp a, prefixes), ipts')
    | parse_prefix(stack, prefixes, RATOR $ :: ipts') =
 	if is_prefix $ then 
	  parse_prefix(stack, $ :: prefixes, ipts')
        else
          raise ParseError("%s is not a prefix operator", [$])
    | parse_prefix(_, _, []) = raise ParseError ("premature EOF", [])
  and parse_postfix(stack, (e, []), 
                    ipts as RATOR (irator as (_, _, POSTFIX)) :: ipts') =
        if noparens(srator stack, irator, LEFT) then (* reduce infix on stack *)
          case stack
            of BIN ($, e', prefixes, stack') =>
			parse_postfix(stack', (BINARY(e', $, e), prefixes), ipts)
             | BOT => raise Impossible (*  BOT has lowest precedence *)
        else if noparens(irator, srator stack, RIGHT) then (* reduce postfix *)
           parse_postfix(stack, (UNARY(irator, e), []), ipts')
        else
           raise ParseError ("%s is non-associative", [irator])
    | parse_postfix(stack, (e, []), 
                    ipts as RATOR (irator as (_, _, INFIX _)) :: ipts') =
        if noparens(srator stack, irator, LEFT) then (* reduce *)
          case stack
            of BIN ($, e', prefixes, stack') =>
			parse_postfix(stack', (BINARY(e', $, e), prefixes), ipts)
             | BOT => raise Impossible (*  BOT has lowest precedence *)
        else if noparens(irator, srator stack, RIGHT) then (* shift *)
           parse_prefix(BIN(irator, e, [], stack), [], ipts')
        else
           raise ParseError ("%s is non-associative", [irator])
    | parse_postfix(stack, (e, $ :: prefixes), 
                    ipts as RATOR (irator as (_, _, POSTFIX)) :: ipts') =
        if noparens($, irator, NONASSOC) then (* reduce prefix rator *)
	  parse_postfix(stack, (UNARY($, e), prefixes), ipts)
        else if noparens(irator, $, NONASSOC) then (* reduce postfix rator *)
	  parse_postfix(stack, (UNARY(irator, e), $ :: prefixes), ipts')
	else
	  raise ParseError 
	    ("can't parse (%s e %s); operators have equal precedence", [$, irator])
    | parse_postfix(stack, (e, $ :: prefixes), 
                    ipts as RATOR (irator as (_, _, INFIX _)) :: ipts') =
        if noparens($, irator, NONASSOC) then (* reduce prefix rator *)
	  parse_postfix(stack, (UNARY($, e), prefixes), ipts)
        else if noparens(irator, $, NONASSOC) then (* shift, look for prefixes *)
	  parse_prefix(BIN(irator, e, $ :: prefixes, stack), [], ipts')
	else
	  raise ParseError 
	    ("can't parse (%s e %s ...); operators have equal precedence", [$, irator])
    | parse_postfix(stack, (e, prefixes), ipts) =
          parse_postfix(stack, (e, prefixes), RATOR juxtarator :: ipts)
  and parse(IMAGE(l)) = parse_prefix(BOT, [], l)
  and exp (ATOMimage a) = ATOMexp a
    | exp (PARENS im)   = parse im
in
  val parse = parse
end  
fun image a = IMAGE [ATOMIC (ATOMimage a)]
fun parenthesize image = IMAGE [ ATOMIC (PARENS image) ]
fun infix_image(IMAGE l, $, IMAGE r) = IMAGE (l @ RATOR $ :: r)
fun bracket((inner, image), side, outer) =
  if noparens(inner, outer, side) then image else parenthesize image
local
  val maxrator = ("<maximum-precedence operator>", maxprec, INFIX NONASSOC)
  fun unparse (ATOMexp a) = (maxrator, image a)
    | unparse (BINARY(l, $, r)) =
        let val l' = bracket (unparse l, LEFT,  $)
            val r' = bracket (unparse r, RIGHT, $)
        in  ($, infix_image(l', $, r'))
        end
    | unparse (UNARY($, e)) =
        let val e' = bracket (unparse e, NONASSOC, $)
            val empty = IMAGE []
        in  ($, if is_prefix $ then infix_image(empty, $, e') 
                else                infix_image(e', $, empty))
        end
in
  val unparse = fn exp => #2 (unparse exp)
end

infix 7 **
infix 6 ++
infix 4 ==
infixr 3 ::==
val mul  = ("*",  7, INFIX LEFT)
val add  = ("+",  6, INFIX LEFT)
val eq   = ("=",  4, INFIX NONASSOC)
val gets = (":=", 3, INFIX RIGHT)
val ince = ("++", 9, PREFIX)
val inco = ("++", 9, POSTFIX)
val dece = ("--", 9, PREFIX)
val deco = ("--", 9, POSTFIX)
val star = ("*",  8, PREFIX)

fun binary $ (l, r) = BINARY (l, $, r)
fun unary $ e = UNARY($, e)

val op **   = binary mul
val op ++   = binary add
val op ==   = binary eq
val op ::== = binary gets
val dece = unary dece
val deco = unary deco
val star = unary star

fun imstring (IMAGE l) = imstring' l
and imstring' (RATOR ($ as (t, _, INFIX _)) :: l) = " " ^ t ^ " " ^ imstring' l
  | imstring' (RATOR ($ as (t, _, _)) :: l) = t ^ imstring' l
  | imstring' (ATOMIC a :: l) = astring a ^ imstring' l
  | imstring' [] = ""
and astring (ATOMimage a) = a
  | astring (PARENS im) = "(" ^ imstring im ^ ")"

val x = ATOMexp "x"
val y = ATOMexp "y"
val z = ATOMexp "z"
fun E (n:int) = ATOMexp (Int.toString n)

val junk = imstring (unparse (z ::== y ::== x ++ y ** z ++ E 2 == E 99 == x))
val junk' = imstring (unparse (z ::== y ::== (x ++ y) ** (z ++ E 2) == E 99 == x))
val left = imstring (unparse ( (x ++ y) ++ z ))
val right = imstring (unparse ( x ++ (y ++ z )))
val left' = imstring (unparse ( (x ::== y) ::== z ))
val right' = imstring (unparse ( x ::== (y ::== z )))
val left'' = imstring (unparse ( (x == y) == z ))
val right'' = imstring (unparse ( x == (y == z )))

val pp = imstring (unparse (E 2 ++ star (deco x)))
val pp' = imstring (unparse (E 2 ++ deco (star x)))
