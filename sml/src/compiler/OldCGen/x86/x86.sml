(* x86.sml,
 * derived from i386.sml
 * by Yngvi Guttesen (ysg@id.dth.dk) and Mark Leone (mleone@cs.cmu.edu)
 *
 * Copyright 1989 by	  Department of Computer Science, 
 *			  The Technical University of Denmak
 *			  DK-2800 Lyngby 
 *
 *)

functor X86CM (V : X86CODER) : CMACHINE = struct

  structure D = X86Spec.ObjDesc

  val dtoi = LargeWord.toInt	(* convert object descriptor to int *)

  structure P = CPS.P
  structure V' :
      sig

	  type Label = V.Label

	  datatype Size = SevenBits |         (* used only for Immed32 *)
			  Byte | Word | Long 

	  datatype EA = Direct of int
		      | Displace of int * int
		      | Index of int * int * int * Size
		      | Immed of int
		      | Immed32 of Word32.word
		      | Immedlab of Label
		      | Floatreg of int

	  val eax : int	(* = 0 *)
	  val ebx : int	(* = 3 *)
	  val ecx : int	(* = 1 *)
	  val edx : int	(* = 2 *)
	  val esi : int	(* = 6 *)
	  val edi : int	(* = 7 *)
	  val ebp : int	(* = 5 *)
	  val esp : int	(* = 4 *)

      end = V

  open V'

  (************************** Register definitions ******************************
   * The 80386 only has 7 general purpose registers.  A stack frame is used
   * to hold other values as needed (see runtime/X86.prim.s), but nothing
   * is ever pushed on the stack, since doing so would invalidate the
   * offsets of values in the stack frame.  This file must agree with the
   * following runtime files: X86.prim.s, run_ml.c, ml_state.h, fpregs.h
   *
   * The choice of which values go in registers can make a big (10-20%)
   * speed difference.  Instructions that indirect through %esp are 1 byte
   * longer and 1 cycle slower than most other other register indirects, so
   * the stack frame isn't all that fast.
   *)

  val tempreg'	     = eax
  val tempreg	     = Direct tempreg'
  val tempmem	     = Displace (esp, 0)
  val tempmem2	     = Displace (esp, 4)
  val exnptr	     = Displace (esp, 8)
  val limitptr	     = Displace (esp, 12)
  val standardclosure  = Displace (esp, 16)
  val standardlink     = Displace (esp, 20)
  val storeptr	     = Displace (esp, 24)
  val varptr	     = Displace (esp, 28)
  val varptr_indexable = false
  val start_gc	     = Displace (esp, X86Spec.startgcOffset)
  val mask	     = Displace (esp, 36)

  (* vregs start at Displace(esp,40)  
     -- see X86.prim.asm, ml-state.c, ml-state.h
  *)
  val numVregs = 12
  local fun mkvreglist 0 = []
	  | mkvreglist n = Displace(esp,(numVregs-n)*4+40) :: mkvreglist (n-1)
  in 
      val vregs = mkvreglist numVregs
  end

  (* pseudo regs are at 88 and 92 *)
  val pseudoOffset = 88
  val pseudo1 = Displace(esp,88)
  val pseudo2 = Displace(esp,92)

  val allocptr'	     = edi
  val allocptr	     = Direct allocptr'
  val arithtemps     = []

  (* ecx must be a misc reg -- see X86.prim.s. *)
  val miscregs	     = map Direct [ebx, ecx, edx] @ vregs

  val standardarg     = Direct ebp	(* NB: instructions w/ebp are longer *)
  val standardcont    = Direct esi

  (* All floating point "registers" are caller-save. *)
  val savedfpregs    = map Floatreg [0,1,2,3,4,5,6]
  val floatregs	     = []

  (*****************************************************************************)

  val comment = V.comment
  val immed = Immed	(* make the immediate integer mode *)
  val immed32 = Immed32
  val align = V.align	(* ensure that next code is on 4-byte boudary *)
  val mark  = V.mark	(* insert a gc-tag in the code so that next
			     address may be moved into a record. *)

  val emitlong   = V.emitlong   (* put a 4-byte integer literal into the code *)
  exception BadReal of string
  val realconst  = V.realconst  (* put a floating literal into the code	    *)
  val emitstring = V.emitstring (* put a literal string into the code
				   (just the chars , no descriptor or length) *)

  (****************************** Labels ***************************************)

  fun newlabel () = Immedlab (V.newlabel ()) 
	       (* create a new label (but don't define it) *) 

  fun die s = ErrorMsg.impossible ("x86/x86.sml: " ^ s)

  fun emitlab (i,Immedlab lab) = V.emitlab (i,lab)
    | emitlab _ = die "emitlab: bad args"
	       (* L3: emitlab (k,L2) is equivalent to L3: emitlong (L2+k-L3) *)

  fun define (Immedlab lab) = V.define lab
    | define _ = die "define: bad arg"
	       (* Associate a label with a point in the code *)

  (******************************* Move ****************************************)

  fun beginStdFn _ = ()

  fun inEA (Direct r, r') = (r = r')
    | inEA (Displace (r, _), r') = (r = r')
    | inEA (Index (r, _, r', _), r'') = (r = r'') orelse (r' = r'')
    | inEA _ = false

  fun move (src, dest) =
      if src = dest then ()
      else case (src, dest)
	of (x as Floatreg 0,  y as Floatreg y') => V.fst false y
	 | (x as Floatreg x', y as Floatreg y') => (V.fld x; 
						    V.fst true (Floatreg (y'+1)))
	 | (Floatreg _,	   _)		      => die "move: bad args"
	 | (_,		   Floatreg _)	      => die "move: bad args"
	 | (_,		   Immed _)	      => die "move: bad args"
	 | (_,		   Immed32 _)	      => die "move: bad args"
	 | (_,		   Immedlab _)	      => die "move: bad args"
	 | (x as Immedlab _,  y as Direct _)    => V.lea (x, y)
	 | (x as Immed _	,  y)		      => V.movl (x, y)
	 | (x as Immed32 _,  y)		      => V.movl (x, y)
	 | (x as Direct _	,  y)		      => V.movl (x, y)
	 | (x		,  y as Direct _)     => V.movl (x, y)
	 | (x		,  y)		      => 
	       if inEA (y, tempreg') then die "move: no temporary"
	       else (move (x, tempreg); move (tempreg, y))

  (*************************** Utility functions *******************************)
  (* three opcode (x,y,z) performs the operation: x opcode y -> z for 
   *			COMMUTATIVE opcodes.
   * three' opcode cmps (x,y,z) performs the same except now it compensates
   * the result for commutativity.
   *)
  fun three opcode (x, y, z as Direct _) = 
	      if x=z then opcode (y,z)
	      else if y=z then opcode (x,z)
	      else if x=y then (move (x,z); opcode (z,z))
	      else (move (y,z); opcode (x,z))
    | three opcode (x as Displace _, y as Displace _, z as Displace _) =
	     if x=z then (move (y,tempreg); opcode (tempreg,z))
	     else if y=z then (move (x,tempreg); opcode (tempreg,z))
	     else if x=y then (move (x,tempreg); 
			       opcode (tempreg,tempreg); 
			       move (tempreg,z))
	     else (move (y,tempreg); opcode (x,tempreg); move (tempreg,z))
    | three opcode (x as Displace _, y, z as Displace _) =
	     if x=z then opcode (y,z) 
	     else (move (x,tempreg); opcode (y,tempreg); move (tempreg,z))
    | three opcode (x, y as Displace _, z as Displace _) =
	     if y=z then opcode (x,z) 
	     else (move (y,tempreg); opcode (x,tempreg); move (tempreg,z))
    | three opcode (x, y, z as Displace _) = 
	     (move (y, tempreg); opcode (x,tempreg); move (tempreg,z))
	     (* NB: This increases code size, but decreases memory traffic. *)
    | three _ _ = die "three: bad args"

  fun three' opcode cmps (x, y, z as Direct _) = 
	      if x=z then (opcode (y,z); cmps z)
	      else if y=z then opcode (x,z)
	      else (move (y,z); opcode (x,z))
    | three' opcode cmps (x as Displace _, y as Displace _, z as Displace _) =
	     if x=z then (move (y,tempreg); opcode (tempreg,z); cmps z)
	     else if y=z then (move (x,tempreg); opcode (tempreg,z))
	     else (move (y,tempreg); opcode (x,tempreg); move (tempreg,z))
    | three' opcode cmps (x as Displace _, y, z as Displace _) =
	     if x=z then (opcode (y,z); cmps z)
	     else (move (x,z); opcode (y,z); cmps z)
    | three' opcode _ (x, y as Displace _, z as Displace _) =
	     if y=z then opcode (x,z) else (move (y,z); opcode (x,z))
    | three' opcode _ (x, y, z as Displace _) =
	     (move (y,z); opcode (x,z))
    | three' _ _ _ = die "three': bad args"


  (***************************** Memory check **********************************)
  fun decLimit n = V.subl (Immed n,limitptr)

  fun testLimit () = V.cmpl (limitptr, allocptr)

  (* checkLimit (n, lab):
   * Generate code to see if there is enough free space to allocate n bytes.
   *)

  fun checkLimit (max_allocation, lab, mask_value, rlab, fregs) = 
      let val lab' = V.newlabel ()
      in
	 V.comment ("begin fun, max alloc = "^(Int.toString max_allocation)^"\n");
	 if max_allocation >= 4096
	     then (V.lea (Displace (allocptr', max_allocation - 4096), tempreg);
		   V.cmpl (limitptr, tempreg))
	     else ();
	 V.jb (Immedlab lab');
	 (case fregs of
	      [] => (move (mask_value, mask);
		     move (lab, tempreg);
		     V.jmp start_gc)
	    | _ => (let val len = length fregs
			val floatSz = 8
			val desc = dtoi(D.makeDesc(len * floatSz, D.tag_string))
			val retlab = V.newlabel()
			fun forall ([],_,_) = ()
			  | forall (freg::rest,i,f) = 
			    (f (freg,i);
			     forall (rest,i+8,f))
			fun deposit (Floatreg 0,i) = 
			    V.fst false (Displace(allocptr',i))
			  | deposit (fr,i) = 
			    (V.fld fr;
			     V.fst true (Displace(allocptr',i)))
			fun restore (Floatreg y',i) = 
			    (V.fld (Displace(tempreg',i));
			     V.fst true (Floatreg (y'+1)))
			fun jump (dest as (Immedlab _)) = V.jra dest
			  | jump x = V.jmp x
		    in
			(* build fp record *)
			move(Immed desc,Displace(allocptr',0));
			forall (fregs,4,deposit);
			V.addl(Immed 4,allocptr);

			(* save it in pseudo1 *)
			move(allocptr,pseudo1);

			V.addl(Immed (floatSz * len),allocptr);
			move(mask_value,mask);
			move(Immedlab retlab,tempreg);
			V.jmp start_gc;

			V.define retlab;
			move(pseudo1,tempreg);
			forall (fregs,0,restore);
			testLimit();
			jump rlab    (* don't know what rlab is *)
		    end));
	 V.define lab'
      end

  (************************* Record manipulation *******************************)

  (* record : (EA * CPS.accesspath) list * EA -> unit *)

  fun record (vl, z) =
      let open CPS
	  fun f (Direct r, SELp(j,p)) = f (Displace (r, j*4), p)
	    | f (Immedlab l, p)	    = (move (Immedlab l, tempreg); 
					 f (tempreg,p))
	    | f (x, OFFp 0)	    = if x=tempreg
					then V.stos x
					else (move (x,tempreg); V.stos tempreg)
	    | f (Direct r, OFFp j)    = (V.lea (Displace (r, j*4), tempreg);
					 f (tempreg, OFFp 0))
	    | f (x,p)		    = (move (x, tempreg); f (tempreg,p))
      in
	  app f vl;
	  (case z of
	     (Direct _) => V.lea (Displace (allocptr', ~4*(List.length vl-1)), z)
	  |  _ => (V.lea (Displace (allocptr', ~4*(List.length vl - 1)), tempreg);
		   V.movl (tempreg,z)))
      end

  fun fprecord(tag,vl,z) = 
      let open CPS
	  val floatSz = 8
	  val tagSz = 4
	  val pop = true
	  fun allocEA i = Displace(allocptr',i*floatSz+tagSz)
	  fun f (_,[]) = ()
	    | f (i,(Direct r,SELp(j,OFFp 0))::rest) = 
	      (V.fld (Displace(r,j*floatSz));
	       V.fst pop (allocEA i);
	       f (i+1,rest))
	    | f (i,(Direct r,SELp(j,p))::rest) = f(i,(Displace(r,j*4),p)::rest)
	    | f (i,(Floatreg 0,OFFp 0)::rest) = 
	      (V.fst (not pop) (allocEA i);
	       f(i+1,rest))
	    | f (i,(fr as Floatreg _,OFFp 0)::rest) = 
	      (V.fld fr;
	       V.fst pop (allocEA i);
	       f(i+1,rest))
	    | f (i,(ea,p)::rest) = 
	      (move(ea,tempreg);
	       f(i,(tempreg,p)::rest))
      in
	  three V.orl (allocptr, Immed 4, allocptr); (* align *)
	  move(tag,Displace(allocptr',0));
	  f(0,vl);
	  (case z of 
	       (Direct _) => V.lea (Displace (allocptr',4),z)
	     | _ => (V.lea (Displace (allocptr',4),tempreg); 
		     V.movl(tempreg,z)));
	  V.addl(Immed (tagSz + floatSz * List.length vl),allocptr)
      end

  fun recordcont _ = ErrorMsg.impossible "record_cont not implemented yet"

  (* recordStore (x, y, alwaysBoxed) records a store operation into 
   * mem[x+2*(z-1)].   The flag alwaysBoxed is true if the value stored 
   * is guaranteed to be boxed.
   *)
  (**
  fun recordStore (x, y, _) = record 
      ([(immed(dtoi(D.makeDesc(3, D.tag_record))), CPS.OFFp 0),
	(x, CPS.OFFp 0), 
	(y, CPS.OFFp 0), 
	(storeptr, CPS.OFFp 0)], 
       storeptr)
  **)

  (* recordStore assumes tempreg is free *)
  (**)
  fun recordStore (x, y, _) = 
      let	fun storeListUpdate r = 
	  (move(r,Displace(allocptr',0));
	   move(storeptr,Displace(allocptr',4));
	   move(allocptr,storeptr);
	   V.addl(Immed 8,allocptr))
      in
	  case (x,y) of
	      (Direct r,Immed 1) => storeListUpdate x
	    | (Direct r,Immed i) => 
		  (move(x,tempreg);
		   V.addl(Immed (2*(i-1)),tempreg);
		   storeListUpdate tempreg)
	    | (Direct r1,Direct r2) => 
		  (move(y,tempreg);
		   V.addl(Immed ~1,tempreg);
		   V.addl(tempreg,tempreg);
		   V.addl(x,tempreg);
		   storeListUpdate tempreg)
	    | (Displace _,Immed 1) => storeListUpdate x
	    | (Displace _,Immed i) => 
		  (move(x,tempreg);
		   V.addl(Immed(2*(i-1)),tempreg);
		   storeListUpdate tempreg)
	    | (Displace _,_) => 
		  (move(y,tempreg);
		   V.addl(Immed ~1,tempreg);
		   V.addl(tempreg,tempreg);
		   V.addl(x,tempreg);
		   storeListUpdate tempreg)
	    | (_,Displace _) => 
		  (move(y,tempreg);
		   V.addl(Immed ~1,tempreg);
		   V.addl(tempreg,tempreg);
		   V.addl(x,tempreg);
		   storeListUpdate tempreg)
	    | _ => die "record store: bad args"
      end
  (**)

  (* select (i, x, y) generates code for y <- mem[x+4*i]. *)
  fun select(i, Direct s, y)	    =  move (Displace (s, i*4), y)
    | select(i, x as Displace _, y)   = (move (x,tempreg); select(i, tempreg, y))
    | select(i, lab as Immedlab _, y) = (move(lab, tempreg); select(i,tempreg,y))
    | select _ = die "select: bad args"

  fun handlepseudo f (x,Immed 1) = f(pseudo1,x)
    | handlepseudo f (x,Immed 3) = f(pseudo2,x)
    | handlepseudo f (x as Direct _,y) = 
      (* y contains '1' for pr 1, and '3' for pr 2 *)
      (V.lea(Displace(esp,pseudoOffset-2),tempreg);  (* compensate for ints *)
       V.addl(y,tempreg);
       V.addl(y,tempreg);
       f (Displace(tempreg',0),x))
    | handlepseudo f (x,y) = 
      (* y contains '1' for pr 1, and '3' for pr 2 *)
      let val temp = allocptr
      in
	  V.lea(Displace(esp,pseudoOffset-2),tempreg);  (* compensate for ints *)
	  V.addl(y,tempreg);
	  V.addl(y,tempreg);
	  V.push temp;          (* can't use esp (w/o fixup) until pop *)

	  (* fixup esp so x can be accessed; this is a hack *)
	  V.addl(Immed 4,Direct esp);  

	  move(x,temp);
	  f (Displace(tempreg',0),temp);

	  V.addl(Immed ~4,Direct esp); (* restore esp for pop *)
	  V.pop temp
       end

  val loadpseudo = handlepseudo move
  val storepseudo = handlepseudo (fn (x,y) => move(y,x))

  (* offset (i, x, y) generates code for y <- x+4*i. *)
  fun offset (i,Direct s,y as Direct _)	     = V.lea (Displace (s,i*4),y)
    | offset (i,Direct s,y)		     = (V.lea(Displace(s,i*4),tempreg);
						  move (tempreg, y))
    | offset (i,x as Displace _,y as Direct _) = (move (x,tempreg); 
						  offset (i, tempreg, y))
    | offset (i,x as Displace _,y)	     = (move (x, tempreg);
						  offset (i, tempreg, tempreg);
						  move (tempreg, y))
    | offset _ = die "offset: bad args"

  (****************** Indexed fetch and store (byte) ***************************)
  (*
   * fetchindexb (x:EA, y:EA, z:EA) fetches a byte: mem[x+z] -> y
   *				 y CAN be x or z 
   * 
   * storeindexb (x:EA, y:EA, z:EA) stores a byte: x -> mem[y+z]
   *)
  fun fetchindexb (x, y as Displace _, z) = (fetchindexb (x,tempreg,z); 
					     move (tempreg,y))
    | fetchindexb (x, y as Direct _, z) =
      (case (x,z) 
	 of (Direct x', Direct z')   =>  V.movzx (Index (x',0,z',Byte), y)
	  | (Direct x', Immed i)	   =>  V.movzx (Displace (x', i),y)
	  | (Direct x', Displace _)  => (V.movl (z, tempreg);
					 V.movzx (Index (x',0,tempreg',Byte),y))
	  | (Displace _, Direct z')  => (V.movl (x, tempreg); 
					 V.movzx (Index (tempreg',0,z',Byte),y))
	  | (Displace _, Immed i)	   => (V.movl (x, tempreg);
					 V.movzx (Displace (tempreg', i), y))
	  | (Displace _, Displace _) => (V.movl (x,tempreg);
					 V.addl (z,tempreg);
					 V.movzx (Displace (tempreg',0),y))
	  | _ => die "fetchindexb: bad args")
    | fetchindexb _ = die "fetchindexb: bad args"

  (* storeindexb (x,y,z) stores a byte: x -> mem[y+z]
   * The 80386 can only perform byte operations on the al,bl,cl,dl,
   * ah,bh,ch, and dh. When doing byte operations on ebp, esi, and edi 
   * (Direct i where i>3) we must use a temporary register.
   *)
  fun storeindexb (x, y, z) =
      let 
	  (* storeb assumes tempreg is free. *)
	  fun storeb (x as Immed _, y) = V.movb (x,y)
	    | storeb (x as Direct x', y) = 
		 if (x' > 3) then (move (x, tempreg); V.movb (tempreg, y))
		 else V.movb (x,y)
	    | storeb (x, y) = (move (x, tempreg); V.movb (tempreg, y))

	  (* storeb' assumes tempreg appears in the EA denoted by y. *)
	  fun storeb' (x,y) = 
	      let val ecx = Direct ecx
		  fun usetemp (x,y) = 
		  (V.movl (ecx, tempmem);	(* Save ecx in memory. *)
		   V.lea (y, tempreg);	(* ecx may appear in x and/or y. *)
		   move (x, ecx);		(* Won't nuke tempreg. *)
		   V.movb (ecx, Displace (tempreg',0));
		   V.movl (tempmem, ecx))
	      in
		  case x 
		    of Immed _ => V.movb (x,y)
		     | Direct i => if i > 3 then usetemp (x,y) else V.movb (x,y)
		     | _ => usetemp (x,y)
	      end
      in
	case (y, z)
	  of (Direct y',	Direct z')  =>	storeb (x, Index (y',0,z',Byte))
	   | (Direct y',	Immed i)    =>	storeb (x, Displace (y',i))
	   | (Direct y',	Displace _) => (V.movl (z,tempreg);
  (* was:					storeb (x,Index (y',0,tempreg',Byte)))
     -lfh *)
					  storeb' (x,Index (y',0,tempreg',Byte)))
	   | (Displace _, Direct z')  => (V.movl (y,tempreg);
					  storeb' (x,Index (tempreg',0,z',Byte)))
	   | (Displace _, Immed i)    => (V.movl (y,tempreg);
					  storeb' (x,Displace (tempreg',i)))
	   | (Displace _, Displace _) => (V.movl (y,tempreg);
					  V.addl (z,tempreg);
					  storeb' (x,Displace (tempreg',0)))
	   | _ => die "storeindexb: bad args"
      end

  (************ Indexed fetch and store (word = 4 byte) ************************)
  (* fetchindexl (x,y,z) fetches a word:	    mem[x+2*(z-1)] -> y	 
   *
   * storeindexl (x,y,z) stores a word:	    x -> mem[y+2*(z-1)]
   *)

  fun fetchindexl (x, y as Displace _, z) = (fetchindexl (x, tempreg, z);
					     move (tempreg, y))
    | fetchindexl (x, y as Direct y', z) =
     (case (x,z) 
      of (Direct x',  Direct z')	=>  V.movl (Index (x', ~2, z', Word), y)
       | (Direct x',  Immed i)	=>  V.movl (Displace (x', 2*(i-1)), y)
       | (Direct x',  Displace _) => (V.movl (z,tempreg);
				      V.movl (Index (x', ~2, tempreg', Word), y))
       | (Displace _, Direct z')	=> (V.movl (x,tempreg);
				      V.movl (Index (tempreg', ~2, z', Word), y))
       | (Displace _, Immed i)	=> (V.movl (x,tempreg);
				      V.movl (Displace (tempreg', 2*(i-1)), y))
       | (Displace _, Displace _) => (V.movl (z,tempreg);
				      V.lea (Index (tempreg',~2,tempreg',Byte), 
					     tempreg);
				      V.addl (x,tempreg);
				      V.movl (Displace (tempreg',0), y))
       | (Immedlab _, Direct z')	=> (move (x,tempreg);
				      V.movl (Index (tempreg', ~2, z', Word), y))
       | (Immedlab _, Immed i)	=> (move (x,tempreg);
				      V.movl (Displace (tempreg',2*(i-1)), y))
       | (Immedlab _, Displace _) => (* This is awkward with only 1 temp. *)
				     (move (x,tempreg);
				      V.addl (z,tempreg);
				      V.addl (z,tempreg);
				      V.movl (Displace (tempreg',~2), y))
       | _ => die "fetchindexl: bad args")
    | fetchindexl _ = die "fetchindexl: bad args"

  (* storeindexl (x,y,z) stores a word:	  x -> mem[y+2*(z-1)]	 *)
  fun storeindexl (x, y, z) =
      let 
	  val ecx = Direct ecx

	  (* move' assumes tempreg appears in the EA denoted by y. *)
	  fun move' (x as Immed _, y) = V.movl (x,y)
	    | move' (x as Direct x', y) = V.movl (x,y)
	    | move' (x, y) = 
		 (V.lea (y, tempreg);
		  V.movl (ecx, tempmem);
		  move (x, ecx);		(* This won't nuke tempreg *)
		  V.movl (ecx, Displace (tempreg',0));
		  V.movl (tempmem, ecx))
      in
	case (y, z)
	  of (Direct y',	Direct z')  =>	move (x, Index (y', ~2, z', Word))
	   | (Direct y',	Immed i)    =>	move (x, Displace (y', 2*(i-1)))
	   | (Direct y',	Displace _) => (move (z, tempreg);
					  move' (x, Index (y',~2,tempreg',Word)))
	   | (Displace _, Direct z')  => (move (y, tempreg);
					  move' (x, Index (tempreg',~2,z',Word)))
	   | (Displace _, Immed i)    => (move (y, tempreg);
					  move' (x, Displace (tempreg',2*(i-1))))
	   | (Displace _, Displace _) => (move (z, tempreg);
					  V.asll (Immed 1, tempreg);
					  V.addl (y, tempreg);
					  move' (x, Displace (tempreg', ~2)))
	   | _ => die "storeindexl: bad args"
      end

  (* fetchindexd (x,y,z): y<-mem[x+4*(z-1)] *)
  (* storeindexd (x,y,z): mem[y+4*(z-1)]<-x *)
  local 
     exception IndexdEA
     fun indexdEA (Direct x', Direct y')	     = Index (x', ~4, y', Long)
       | indexdEA (Direct x', Immed i)	     = Displace (x', 4*(i-1))
       | indexdEA (Direct x', y as Displace _) = 
	 if x' = tempreg' then die "tempreg in use in indexdEA 1"
	 else (V.movl (y, tempreg);
	       Index (x', ~4, tempreg', Long))
       | indexdEA (x as Displace _, Direct y') = 
	 if y' = tempreg' then die "tempreg in use in indexdEA 2"
	 else (V.movl (x, tempreg);
	       Index (tempreg', ~4, y', Long))
       | indexdEA (x as Displace _, Immed i)   = (V.movl (x, tempreg);
						  Displace (tempreg', 4*(i-1)))
       | indexdEA (x as Displace _, y as Displace _) = (V.movl (y, tempreg);
							V.asll (Immed 2,tempreg);
							V.addl (x, tempreg);
							Displace (tempreg', ~4))
       | indexdEA _ = raise IndexdEA
  in
     fun fetchindexd (x, y as Floatreg y', z) = 
	 let val src = indexdEA (x,z) 
	     handle IndexdEA => die "fetchindexd: bad args"
	 in
	     V.fld src;
	     V.fst true (Floatreg (y'+1))
	 end
       | fetchindexd _ = die "fetchindexd: bad args"

     fun storeindexd (x as Floatreg x', y, z) =
	 let val dest = indexdEA (y,z)
	     handle IndexdEA => die "storeindexd: bad args"
	 in
	     if x' = 0 then V.fst false dest
	     else (V.fld x; V.fst true dest)
	 end
       | storeindexd _ = die "storeindexd: bad args"

  end (* local *)

  (******************************** Shifts *************************************)
  (* Only ECX can hold the count in a non-immediate shift.
   * The 80386 only shifts modulo 32 so it is possible that this function	 
   * will lead to an error.  
   *)
  local 
    val ecx' = 1
    val ecx = Direct ecx'
    fun checkCnt' i = if i < 0 then die "shift: negative count"
		      else Immed (Int.min(i,31))
    fun checkCnt (Immed i,x,y) = (checkCnt' i,x,y)
      | checkCnt (Immed32 i,x,y) = (checkCnt' (Word32.toIntX i),x,y)
      | checkCnt x = x
    fun shift opr (i as Immed _, src, dest) = 
	(move (src, dest); opr (i, dest))
      | shift opr (cnt, src, dest as Direct 1) =
	(move (src, tempreg);
	 move (cnt, ecx);
	 opr (ecx, tempreg);
	 move (tempreg, dest))

      | shift opr (cnt as Direct 1, src, dest) =
	(move (src, dest); opr (ecx, dest))

      | shift opr (cnt, src, dest) =
	(* This code is complicated by the fact that cnt, src, and dest 
	 may be EAs involving %ecx, and that cnt may equal dest. *)
	(move (src, tempreg);
	 move (ecx, tempmem);
	 move (cnt, ecx);
	 opr (ecx, tempreg);
	 move (tempmem, ecx);
	 move (tempreg, dest))
  in
      val ashl = (shift V.asll) o checkCnt
      val ashr = (shift V.asrl) o checkCnt
      val lshr = (shift V.lsrl) o checkCnt
  end 

  (*************************** Arithmetic **************************************)

  (****)
  (* We can use lea to speed up additions in which overflow is ignored. *)
  fun add (x, y, z as Direct z') =
	 if (x = Immed 1 orelse x = Immed32 0w1) andalso y = z then V.incl z
	 else if (y = Immed 1 orelse y = Immed32 0w1) andalso x = z then V.incl z
	 else
	  (case (x,y) 
	     of (Direct x', Immed i) => V.lea (Displace (x', i), z)
	      | (Immed i, Direct y') => V.lea (Displace (y', i), z)
	      | (Direct _,Immed32 _) =>  three V.addl (x,y,z)
	      | (Immed32 _, Direct _) => three V.addl (x,y,z)
	      | (Direct x', Direct y') => if x' <> z' andalso y' <> z'
					  then V.lea (Index (x', 0, y', Byte), z)
					  else three V.addl (x,y,z)
	      | _ => three V.addl (x,y,z))
    | add (x,y,z) = 
	 if (x = Immed 1 orelse x = Immed32 0w1) andalso y = z then V.incl z
	 else if (y = Immed 1 orelse y = Immed32 0w1) andalso x = z then V.incl z
	 else if x = y then ashl (Immed 1, x, z)
	 else three V.addl (x,y,z)
  (****)

  (** val add = three V.addl (* for debugging *) **)
  fun addt x = (three V.addl x; V.into ())

  fun sub (x, y, z) = let
    fun sub1(x, z) = V.subl(x,z)
    fun sub2(x, y, z) =			
      (move (y, tempreg);  sub1 (x, tempreg);  move (tempreg, z))
    fun sub3(x, y, z) = (move (y, z);  sub1 (x, z))
    fun sub4(x, z) = (move (x, tempreg);  sub1 (tempreg, z))
  in
    if y = z then
      (case z 
       of Direct _ => sub1(x, z)
        | _ => (case x of Displace _ => sub4(x, z) | _ => sub1(x,z))
       (*esac*))
    else 
      (case z 
       of Direct _ => if x = z then sub2 (x, y, z) else sub3 (x, y, z)
        | _ => sub2 (x, y, z)
       (*esac*))
  end

  fun subt x = (sub x; V.into())

  (* Can't use LEA here because it doesn't set the overflow flag. *)
  fun mull mulFn (src, dest as Direct _) = mulFn (src, dest)
    | mull mulFn (src, dest) = (move (dest, tempreg); 
				mulFn (src, tempreg); 
				move (tempreg, dest))

  (* On the 80386 signed (unsigned) integer division is done with the IDIV (UDIV)
     instruction.	 For IDIV, the dividend is sign-extended into EDX:EAX.
     For UDIV, EDX is zero and the dividend is in EAX. The divisor must 
     be either a register or a memory location.  The quotient is stored 
     in EAX (e.g. tempreg) and the remainder in EDX.  Hence, we must save 
     EDX unless it is the dividend. *)

  local val edx' = 2
	val edx = Direct edx'
  in
  fun divl divFn (x as Immed _, y) = (V.movl (x, tempmem); 
				      divl divFn (tempmem, y))
    | divl divFn (x as Immed32 _, y) = (V.movl (x, tempmem); 
					divl divFn (tempmem, y))
    | divl divFn (x, y) = 
	 let val x = if inEA (x, edx') then (move (x, tempmem); tempmem)
		     else x
	 in
	     V.movl (y, tempreg);		(* NB: y may be an EA involving edx. *)
	     if y = edx then		(* OK to overwrite edx. *)
		 (divFn x;
		  V.movl (tempreg, y))
	     else
		 (* We must save edx, since divFn will destroy it.
		    We can't push it, since x may be an EA involving esp! *)
		 (V.movl (edx, tempmem2);
		  divFn x;
		  V.movl (tempmem2, edx);
		  V.movl (tempreg, y))
	 end
  end

  fun mult x = (mull V.mullExtend x; V.into ())
  fun divt x = (divl (fn y => (V.cdq(); V.idivl y)) x; V.into ())

  (************************** Word32 operations ********************************)
  val mulu = mull V.mull
  val divtu = divl (fn x => (move(Immed 0, Direct edx); V.udivl x))
  (* addu, subu, lshr defined above *)

  (************************** Bitwise operations *******************************)

  fun notb (a,b) = (move (a,b); V.notl b)
  val orb	 = three V.orl	
  val xorb = three V.xorl 
  val andb = three V.andl


  (*************************** Branches ***************************************)
  fun jmp (lab as Immedlab _) = V.jra lab
    | jmp (x   as Direct _)   = V.jmp x
    | jmp (x   as Displace _) = V.jmp x
    | jmp _ = die "jmp: bad arg"

  (* jmpindexb (x,y)     (x+y) -> PC     *)
  fun jmpindexb (lab as Immedlab _, indx as Direct _)   = jmpidx (lab, indx)
    | jmpindexb (lab as Immedlab _, indx as Displace _) = jmpidx (lab, indx)
    | jmpindexb _ = die "jmpindexb: bad arg"

  and jmpidx (lab, indx) = (move (lab, tempreg);
			    V.addl (indx, tempreg);
			    V.jmp tempreg)

  datatype condition = NEQ | EQL | LEQ | GEQ | LSS | GTR 
		     | GEU | GTU | LTU | LEU

  fun cbranch NEQ = V.jne
    | cbranch EQL = V.jeq
    | cbranch LEQ = V.jle
    | cbranch GEQ = V.jge
    | cbranch LSS = V.jlt
    | cbranch GTR = V.jgt
    | cbranch GEU = V.jae  (* above and equal *)
    | cbranch GTU = V.ja   (* above *)
    | cbranch LTU = V.jb   (* below *)
    | cbranch LEU = V.jbe  (* below and equal *)

  fun rev LEQ = GEQ
    | rev GEQ = LEQ
    | rev LSS = GTR
    | rev GTR = LSS
    | rev NEQ = NEQ
    | rev EQL = EQL
    | rev GEU = LEU
    | rev GTU = LTU
    | rev LTU = GTU
    | rev LEU = GEU

  (* if op1 <cond> op2 then label -> PC else () 
   * Note that cmpl (op1,op2) is equivalent to flags = op2-op1
   * that is if we want to see if op1 <= op2 we have to make 
   * the test cmpl (op2,op1) (op1-op2) and jump on the condition leq
   *)
  fun ibranch (cond, op1 as Displace _, op2 as Displace _, label) =
	 (move (op1, tempreg); ibranch (cond, tempreg, op2, label))
    | ibranch (cond, op1 as Immed _, op2 as Immed _, label) =
	 (move (op2,tempreg); V.cmpl (op1, tempreg); cbranch (rev cond) label)
    | ibranch (cond, op1 as Immed _, op2, label)	= 
	 (V.cmpl (op1, op2); cbranch (rev cond) label)
    | ibranch (cond, op1 as Immed32 _, op2 as Immed32 _, label)	= 
	 (move (op2,tempreg);
	  V.cmpl (op1, tempreg); cbranch (rev cond) label)
    | ibranch (cond, op1 as Immed32 _, op2, label)	= 
	 (V.cmpl (op1, op2); cbranch (rev cond) label)
    | ibranch (cond, op1, op2, label)  = 
	 (V.cmpl (op2,op1); cbranch cond label)

  (* bbs (i, dst, lab): test the i'th bit of dst and jump to lab if it is set.
   * This function is only called from one place in GENERIC.SML, and that is 
   * as: bbs (immed 0, regbind x, lab); gen a; genlab (lab, b)
   *)
  fun bbs (x as Immed _, y as Direct _  , l) = (V.btst (x,y);
						V.jc l)
    | bbs (x as Immed _, y as Displace _, l) = (V.btst (x,y);
						V.jc l)
    | bbs _ = die "bbs: bad arg"

  (************************** Floating point instructions *********************)

  (* This code is complicated by the fact that the 80387 coprocessor
     uses a stack of floating point registers.  The top of the stack is an
     implicit argument in most floating point instructions.  We use seven of
     the eight available stack entries as "registers"; the remaining
     entry (at the top of the stack) is used as a temporary. 
     Unfortunately, loading the temporary must be done with a "push", 
     which changes the offsets of the other "registers".	Note that most
     floating point instructions can optionally pop the register stack. *)

  fun loadfloat (x as Direct _, y as Floatreg y') = 
	 fetchindexd (x, y, Immed 1)
    | loadfloat (x, y as Floatreg y') = 
	 (move (x, tempreg);
	  fetchindexd (tempreg, y, Immed 1))
    | loadfloat _ = die "loadfloat: bad args"

  fun storefloat (x as Floatreg x', y) = 
	 (V.movl (Immed(dtoi D.desc_reald), tempreg);
	  V.stos tempreg;
	  storeindexd (x, allocptr, Immed 1);
	  move (allocptr, y);
	  V.addl (Immed 8, allocptr))
    | storefloat _ = die "storefloat: bad args"


  (* float1 opr (x,y) generates code for y <- opr x. *)

  fun float1 opr (x as Floatreg x', y as Floatreg y') = 
	 if x' = y' andalso y' = 0 then opr ()
	 else (V.fld x; opr (); V.fst true (Floatreg (y'+1)))
    | float1 _ _ = die "float1: bad args"

  (* float2 opr (x,y) generates code for y <- x opr y.  The operator
     takes a boolean that specifies whether to pop the register stack. *)

  fun float2 opr (x as Floatreg x', y as Floatreg y') = 
	 if x' = 0 (* orelse y' = 0 *) then opr false (x, y)
	 else (V.fld x; opr true (Floatreg 0, Floatreg (y'+1)))
    | float2 _ _ = die "float2: bad args"

  (* float3 opr b (x,y,z) generates code for z <- x opr y.  b is a
     boolean specifying whether opr is commutative.  The operator takes a
     boolean that specifies whether to pop the register stack. *)

  fun float3 opr commut (x as Floatreg x', y as Floatreg y', z as Floatreg z') =
	 if x' = z' andalso commut then float2 opr (y, z)
	 else if y' = z' then float2 opr (x, z)
	 else (V.fld x;
	       opr false (Floatreg (y'+1), Floatreg 0);
	       V.fst true (Floatreg (z'+1)))
    | float3 _ _ _ = die "float3: floating point register arguments expected"

  val fmuld = float3 V.fmul true
  val fdivd = float3 V.fdiv false
  val faddd = float3 V.fadd true
  val fsubd = float3 V.fsub false
  val fnegd = float1 V.fchs
  val fabsd = float1 V.fabs

  fun cvti2d (x as Direct _, y as Floatreg y') =
	 (V.movl (x, tempmem); 
	  V.fild tempmem;
	  V.fst true (Floatreg (y'+1)))
    | cvti2d (x as Displace _, y as Floatreg y') =
	 (V.fild x;
	  V.fst true (Floatreg (y'+1)))
    | cvti2d _ = die "cvti2d: bad args"

  fun fbranchd (cond, x, y, label) = let
    fun fcom (x as Floatreg x', y as Floatreg y') =
	if x' = 0 then V.fucom false (x, y)
	else (V.fld x; V.fucom true (Floatreg 0, Floatreg (y'+1)))
      | fcom _ = die "fbranchd: bad args"
    fun branch () = let
      fun andil i = V.andl(Immed i, tempreg)
      fun xoril i = V.xorl(Immed i, tempreg)
      fun cmpil i = V.cmpl(Immed i, tempreg)
    in
      (case cond
       of P.fEQ    (* = *) => (andil 0x4400; xoril 0x4000; V.jeq label)
        | P.fULG (* ?<> *) => (andil 0x4400; xoril 0x4000; V.jne label)
        | P.fUN    (* ? *) => (V.sahf(); V.jp label)
        | P.fLEG (* <=> *) => (V.sahf(); V.jnp label)
        | P.fGT    (* > *) => (andil 0x4500;  V.jeq label)
        | P.fULE (* ?<= *) => (andil 0x4500;  V.jne label)
        | P.fGE   (* >= *) => (andil 0x500; V.jeq label)
	| P.fULT  (* ?< *) => (andil 0x500; V.jne label)
	| P.fLT    (* < *) => (andil 0x4500; cmpil 0x100; V.jeq label)
	| P.fUGE (* ?>= *) => (andil 0x4500; cmpil 0x100; V.jne label)
	| P.fLE   (* <= *) => 
	   (andil 0x4100; cmpil 0x100; V.jeq label; cmpil 0x4000; V.jeq label)
	| P.fUGT  (* ?> *) => 
	   (V.sahf(); V.jp label; andil 0x4100; V.jeq label)
	| P.fLG   (* <> *) => (andil 0x4400; V.jeq label)
	| P.fUE   (* ?= *) => (andil 0x4400; V.jne label)
      (*esac*))
    end
  in fcom (x,y); V.fnstsw(); branch()
  end
end (* functor X86CM *)


(*
 * $Log: x86.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:49  george
 * Version 110.5
 *
 *)
