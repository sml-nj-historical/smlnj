(* Copyright 1996 by AT&T Bell Laboratories *)
(* sympaths.sml *)

structure SymPath : SYMPATH =
struct

  structure S = Symbol

  datatype path = SPATH of S.symbol list

  exception SymPath

  val empty = SPATH nil

  fun null(SPATH p) = List.null p

  fun extend(SPATH p: path, s: S.symbol) = SPATH(p @ [s])

  fun prepend(s: S.symbol, SPATH p: path) = SPATH(s::p)

  fun append(SPATH front: path, SPATH back: path) = SPATH(front @ back)

  fun first(SPATH []: path) = raise SymPath
    | first(SPATH(s::_)) = s

  fun rest(SPATH []: path) = raise SymPath
    | rest(SPATH(_::p)) = SPATH p

  fun length(SPATH p: path) = List.length p

  (* the last element of a path *)
  fun last(SPATH p) =
      List.last p
	handle List.Empty => ErrorMsg.impossible "SymPath.last"

  fun equal(SPATH p1: path, SPATH p2: path) = (p1 = p2)

  val resultId = Symbol.strSymbol "<resultStr>"
  val returnId = Symbol.strSymbol "<returnStr>"

  fun toString(SPATH p: path) =
     let fun f [s] = [Symbol.name s]
	   | f (a::r) = 
                 if (Symbol.eq(a,resultId)) orelse
                    (Symbol.eq(a,returnId)) 
                 then f r
                 else Symbol.name a :: "." :: f r
	   | f nil = ["<empty spath>"]
      in concat(f p)
     end

end (* structure SymPath *)


structure InvPath : INVPATH =
struct

  structure S = Symbol

  datatype path = IPATH of S.symbol list

  exception InvPath

  val empty = IPATH nil

  fun null(IPATH p) = List.null p

  fun extend(IPATH p: path, s: S.symbol) = IPATH(s::p)

  fun append(IPATH front: path, IPATH back: path) = IPATH(back @ front)

  fun last(IPATH []: path) = raise InvPath
    | last(IPATH(s::_)) = s

  fun lastPrefix(IPATH []: path) = raise InvPath
    | lastPrefix(IPATH(_::p)) = IPATH p

  fun equal(IPATH p1:path, IPATH p2:path) = (p1 = p2)

  fun toString(IPATH p: path) =
     let fun f [s] = [Symbol.name s, ">"]
	   | f (a::r) = Symbol.name a :: "." :: f r
	   | f nil = [">"]
      in concat("<" :: f p)
     end

end (* structure InvPath *)


structure ConvertPaths : CONVERTPATHS =
struct

  type spath = SymPath.path
  type ipath = InvPath.path

  fun invertSPath(SymPath.SPATH p : SymPath.path) : InvPath.path =
      InvPath.IPATH(rev p)
  fun invertIPath(InvPath.IPATH p : InvPath.path) : SymPath.path =
      SymPath.SPATH(rev p)

end (* structure ConvertPaths *)
