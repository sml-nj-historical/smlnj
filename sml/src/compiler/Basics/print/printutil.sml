(* Copyright 1989 by AT&T Bell Laboratories *)
(* printutil.sml *)

structure PrintUtil : PRINTUTIL = 
struct

  val say = Control_Print.say

  structure Symbol : SYMBOL = Symbol

  fun newline () = say "\n"
  fun tab 0 = () | tab n = (say " "; tab(n-1))

  fun printSequence (separator: string) pr elems =
      let fun prElems [el] = pr el
	    | prElems (el::rest) = (pr el; say separator; prElems rest)
	    | prElems [] = ()
       in prElems elems
      end

  fun printClosedSequence (front: string, sep, back:string) pr elems =
      (say front; printSequence sep pr elems; say back)

  fun printSym(s: Symbol.symbol) = TextIO.print(Symbol.name s)
      (* fix -- maybe this belongs in Symbol *)

  fun formatQid p =
    let fun f [s] = [Symbol.name s]
          | f (a::r) = Symbol.name a :: "." :: f r
	  | f nil = ["<bogus qid>"]
     in concat(f p)
    end

  fun trimmed (s, maxsz) =
      if size s <= maxsz then s
      else String.substring (s, 0, maxsz) ^ "#"

  fun mlstr s = concat ["\"", String.toString s, "\""]
  fun pr_mlstr s = mlstr (trimmed (s, !Control_Print.stringDepth))
  fun pr_intinf i = trimmed (IntInf.toString i, !Control_Print.intinfDepth)

  fun nlindent n = (newline(); tab n)

  fun printvseq ind (sep:string) pr elems =
      let fun prElems [el] = pr el
	    | prElems (el::rest) = (pr el; nlindent ind; say sep; prElems rest)
	    | prElems [] = ()
       in prElems elems
      end

  (* debug print functions *)
  val prIntPath = printClosedSequence ("[",",","]") (say o Int.toString)
  val prSymPath = printSequence "." printSym

end (* structure PrintUtil *)

