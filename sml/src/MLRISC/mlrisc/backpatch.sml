(* Copyright 1989 by AT&T Bell Laboratories 
 *
 *)
signature BACKPATCH =
sig
    structure I : INSTRUCTIONS
    val emitString : string -> unit
    val defineLabel : Label.label -> unit
    val align : unit -> unit
    val emitSdi : I.instruction -> unit
    val mark : unit -> unit
    val finish : unit -> Word8Vector.vector
end

signature JUMPS =
sig
  structure I : INSTRUCTIONS
  val sdiSize : I.instruction *  (Label.label -> int) * int -> int
  val emitJump : I.instruction * int -> string
  val emitLong : int -> string
end

functor Backpatch(Kind: JUMPS) : BACKPATCH =
struct 
  structure I = Kind.I
  open Kind System.Tags

  datatype Desc =
      BYTES of string * Desc 
    | JUMP of I.instruction * Label.label * int ref * Desc
    | LABEL of Label.label * Desc
    | ALIGN of Desc
    | MARK of Desc 
    | NIL

  fun compress(len, sl as [s], r0 as BYTES(t,r)) =
             let val lent = size t
              in if len+len > lent andalso lent < 500 andalso len<500
                   then compress(len+lent, t::sl, r)
		   else BYTES(s, r0)
             end
    | compress(len, sl, r0 as BYTES(t,r)) =
             let val lent = size t
              in if len+len > lent andalso lent < 500
                   then compress(len+lent, t::sl, r)
		   else BYTES(concat sl, r0)
             end
    | compress(len, sl, r0) = BYTES(concat sl, r0)

  val refs = ref NIL
  fun emitString s = refs := compress(size s, [s],!refs)
  fun align() = refs := ALIGN(!refs)
  fun mark() = refs := MARK(!refs)
  fun defineLabel lab = refs := LABEL(lab, !refs)
  fun emitSdi instr = refs := JUMP(instr, ref 0, !refs)

  fun reverse(r,NIL) = r
    | reverse(r,BYTES(s,q)) = reverse(BYTES(s,r),q)
    | reverse(r,ALIGN q) = reverse(ALIGN r, q)
    | reverse(r,MARK q) = reverse(MARK r, q)
    | reverse(r,LABEL(lab,q)) = reverse(LABEL(lab,r), q)
    | reverse(r,JUMP(instr,s, q)) = reverse(JUMP(instr,s,r),q)

  fun finish() =
   let val changed = ref true

       fun labels (pos, BYTES(s,rest)) = labels(pos+size s,rest)
         | labels (pos, JUMP(_, ref size, rest)) = labels(pos+size, rest)
	 | labels (pos, LABEL(l,rest)) = (l := pos; labels(pos,rest))
	 | labels (pos, lab as ALIGN rest) = labels(((pos+3)div 4)*4, rest)
	 | labels (pos, MARK rest) = labels(pos+4, rest)
	 | labels (pos, NIL) = ()

       fun adjust (pos, BYTES(s,rest)) = adjust(pos+size s,rest)
	 | adjust (pos, JUMP(instr, r as ref size, rest)) =
		let val s = sdiSize(instr, Label.addrOf, pos)
	        in  if s > size then (r := s; changed := true) else ();
                    adjust(pos+size, rest)
		end
	 | adjust (pos, LABEL(l,rest)) = adjust(pos,rest)
	 | adjust (pos, ALIGN rest) = adjust(((pos+3)div 4)*4, rest)
	 | adjust (pos, MARK rest) = adjust(pos+4, rest)
	 | adjust (pos, NIL) = ()

       fun chunk(pos, BYTES(s,r)) = s :: chunk(pos+size s,r)
	 | chunk(pos, JUMP(instr, ref size, r)) =
	        emitJump(instr,size) :: chunk(pos+size,r)
	 | chunk(pos, LABEL(l, rest)) = chunk(pos,rest)
	 | chunk(pos, ALIGN rest) =
		(case pos mod 4
		  of 0 => chunk(pos,rest)
		   | 1 => "\000\000\000" :: chunk(pos+3,rest)
		   | 2 => "\000\000" :: chunk(pos+2,rest)
		   | 3 => "\000" :: chunk(pos+1,rest))
	 | chunk(pos, MARK r) =
     	       emitLong(make_desc((pos+4)div 4,tag_backptr)) :: chunk(pos+4, r)
	 | chunk(pos, NIL) = nil

       val reflist = reverse (ALIGN NIL, !refs) before refs := NIL
    in  while !changed
	   do (changed := false; labels(0, reflist); adjust(0, reflist));
	Byte.stringToBytes (concat (chunk(0, reflist)))
   end
end (* functor BackPatch *)


(*
 * $Log$
 *)
