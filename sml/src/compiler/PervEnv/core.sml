(* core.sml
 *
 * COPYRIGHT 1989 by AT&T Bell Laboratories 
 *
 * Core assumes that the following bindings are already in the static 
 * environment: 
 *
 *   1. Built-in structures, defined in PrimTypes (env/prim.sml): 
 *        PrimTypes InLine
 *   
 *   2. Built-in type constructors, defined in PrimTypes (env/prim.sml): 
 *        int string bool unit real list array ref exn
 *
 *   3. Built-in data constructors, also from PrimTypes (env/prim.sml):
 *        :: nil ref true false
 *
 *   4. Built-in primitive operators, defined in InLine (env/prim.sml).
 *      The InLine structure is not typed (all values have type alpha, this
 *      will change in the future though !). 
 *       
 *   5. The Assembly structure: its static semantics is defined by elaborating
 *      the boot/dummy.sml file, and its dynamic semantics is directly coming
 *      the implementation module provided by the runtime system.
 *
 * In addition, all matches in this file should be exhaustive; the match and 
 * bind exceptions are not defined at this stage of bootup, so any uncaught 
 * match will cause an unpredictable error. 
 *
 *)

structure Core = 
  struct
  (* 
   * We build an Assembly structure from the implementation module provided 
   * from the runtime systems. The coercions are implemented via InLine.cast, 
   * a primitive operator hardwired inside the compiler. In the future, the 
   * linkage should be done safely without using cast (ZHONG).
   *
   * Note: in the future, the Assembly.A substructure will be replaced by
   * a dynamic run vector (JHR).
   *)
    structure Assembly : ASSEMBLY = 
      struct
	open Assembly

	val cast : 'a -> 'b = InLine.cast  
        datatype ('a, 'b) pair = PAIR of 'a * 'b

        structure A = 
          struct
	    structure AA = Assembly.A

	    type c_function = AA.c_function
	    type word8array = AA.word8array
	    type real64array = AA.word8array
	    type spin_lock = AA.spin_lock

            val arrayP : (int, 'a) pair -> 'a array = cast AA.array
	    val array : int * 'a -> 'a array = fn x => arrayP(PAIR x)
                  
            val bind_cfunP : (string, string) pair -> c_function = 
                     cast AA.bind_cfun
	    val bind_cfun : (string * string) -> c_function = 
                     fn x => bind_cfunP (PAIR x)

            val callcP : (c_function, 'a) pair -> 'c = cast AA.callc
	    val callc : (c_function * 'a) -> 'c = fn x => callcP (PAIR x)

	    val create_b : int -> word8array = cast AA.create_b
	    val create_r : int -> real64array = cast AA.create_r
	    val create_s : int -> string = cast AA.create_s
            val create_vP : (int, 'a list) pair -> 'a vector = cast AA.create_v
	    val create_v : int * 'a list -> 'a vector = 
                     fn x => create_vP(PAIR x)

	    val floor : real -> int = cast AA.floor
	    val logb : real -> int = cast AA.logb
            val scalbP : (real, int) pair -> real = cast AA.scalb
	    val scalb : real * int -> real = fn x => scalbP(PAIR x)

	    val try_lock : spin_lock -> bool = cast AA.try_lock
	    val unlock : spin_lock -> unit = cast AA.unlock
           
	  end (* structure A *)

	  val array0 : 'a array = cast array0
	  val vector0 : 'a vector = cast vector0
	  val real64array0 : A.real64array = cast real64array0

      end (* structure Assembly *)

    infix 7  * / quot mod rem div
    infix 6 ^ + -
    infix 3 := o
    infix 4 > < >= <=
    infixr 5 :: @
    infix 0 before

    exception Bind
    exception Match

    exception Range      	(* for word8array update *)
    exception Subscript  	(* for all bounds checking *)
    exception Size 

    local exception NoProfiler
    in val profile_register =
      ref(fn s:string => (raise NoProfiler):int*int array*int ref)
    end

    local val ieql : int * int -> bool = InLine.i31eq
          val peql : 'a * 'a -> bool = InLine.ptreql
          val ineq : int * int -> bool = InLine.i31ne
          val feql : real * real -> bool = InLine.f64eq
          val boxed : 'a -> bool = InLine.boxed
          val length : 'a -> int = InLine.length
          val op + : int * int -> int = InLine.i31add
          val op - : int * int -> int = InLine.i31sub
          val op * : int * int -> int = InLine.i31mul
          val ordof : string * int -> int = InLine.ordof
          val cast : 'a -> 'b = InLine.cast
          val getObjTag : 'a -> int = InLine.gettag
          val sub : 'a vector * int -> 'a = InLine.vecSub
          val andb : int * int -> int = InLine.i31andb
	  val width_tags = 6  (* 4 tag bits plus "10" *)
	  val lshift : int * int -> int = InLine.i31lshift
          val stringCreate : int -> string = Assembly.A.create_s
          val stringUpdate : string * int * char -> unit = InLine.store
          val stringSub : string * int -> char = InLine.ordof

          (* the type annotation is just to work around an bug - sm *)
          val ltu : int * int -> bool = InLine.i31ltu

     (* all the string literals appeared in this file should be 
      * called by the "dupstring" function first.
      *)
          fun dupstring (a : string) = let val len = length a
              (* unsafe string create, no size check !!! but this is only
	       * used for copying string literals.
	       *)
                val ss = stringCreate (len)
                fun copy n = if ieql(n,len)
		      then ()
                      else (stringUpdate(ss, n, stringSub(a,n)); copy(n+1))
		in
		  copy 0; ss
		end

    in 

     (* limit of array, string, etc. element count is one greater than 
      * the maximum length field value (sign should be 0).
      *)
       val max_length = lshift(1, 31 - width_tags) - 1

       fun mkNormArray (n, init) = 
             if ieql(n, 0) then Assembly.array0
             else if ltu(max_length, n) then raise Size 
                  else Assembly.A.array (n, init)

       val rarray0 : real array = InLine.cast Assembly.real64array0
       val mkrarray : int -> real array = InLine.cast Assembly.A.create_r
       fun mkRealArray(n : int, v : real) = 
             if ieql(n, 0) then rarray0
             else if ltu(max_length, n) then raise Size 
                  else let val x = mkrarray n
                           fun init i = 
                             if ieql(i,n) then x
    			     else (InLine.f64Update(x,i,v); 
                                   init ((op +) (i, 1)))
                        in init 0
                       end

       val vector0 = Assembly.vector0  (* needed to compile ``#[]'' *)

       val dupstring = dupstring

      
	(* ZIDO:  The following definitions are essentially stolen from
                  SMLofNJ.Susp.  Unfortunately, they had to be copied
                  here in order to implement lazyness. *)

        (* (In particular, in order to be able to compute pids for them.) *)

      (* ZIDO:  The following is hard-wiredand needs to track the object
       * descriptor definitions.
       *)
        val TSUS = 0;  (* == ObjectDesc.special_unevaled_susp *)
        val TSES = 1;  (* == ObjectDesc.special_evaled_susp *)

        datatype 'a susp = Something of 'a  (* Just a hack for bootstrapping *)

        fun delay (f : unit -> 'a) = (InLine.mkspecial(TSUS , f)):('a susp)
        fun force (x : 'a susp) =
              if InLine.i31eq((InLine.getspecial x),TSUS)
               then let
                  val y : 'a = InLine.arrSub (InLine.cast x, 0) ()
                  in
                    InLine.arrUpdate (InLine.cast x, 0, y);
                    InLine.setspecial (InLine.cast x, TSES);
                    y
                  end
                else InLine.arrSub (InLine.cast x, 0)

       fun stringequal(a : string,b : string) =
	  if peql(a,b) then true
          else let
	    val len = length a
            in
	      if ieql(len, length b)
                 then let fun f 0 = true
                            | f i = let val j = (op -)(i,1)
                	             in if ieql(ordof(a,j),ordof(b,j))
              	                        then f j else false
              	                    end
	               in f len
                      end
	         else false
	    end

       fun polyequal (a : 'a, b : 'a) = peql(a,b)
	    orelse (boxed a andalso boxed b
	    andalso let val aTag = getObjTag a
		        fun pairEq () = 
                          let val bTag = getObjTag b
		           in (ieql(bTag,0x02) orelse ineq(andb(bTag,0x3),0x2))
			      andalso (let val (xa,ya) = cast a
                                           val (xb,yb) = cast b
                                        in polyequal(xa, xb)
 			                   andalso polyequal(ya, yb)
                                       end)
		          end
		     in case aTag
		         of 0x02 (* tag_pair *) => pairEq()
		          | 0x06 (* tag_reald *) => feql(cast a,cast b)
		          | 0x12 (* tag_special *) => false
		          | 0x22 (* tag_record *) => 
                             if ieql(getObjTag b,aTag)
			     then (ieql(length a,length b) andalso 
                                   (let val x : 'a vector = cast a
                                        val y : 'a vector = cast b
                                        val lenm1 = (length x)-1
         
		  	                fun m (j : int) = 
                                          if ieql(j,lenm1)
				          then polyequal(sub(x,j),sub(y,j))
				          else polyequal(sub(x,j),sub(y,j)) 
                                             andalso m(j+1)
                             (* 
                              * must compare lengths, since vectors 
                              * also use the record tag.
                              *)
                                     in m 0
                                    end))
                             else false
                          | 0x26 (* tag_array *) => false
                          | 0x2a (* tag_string *) => 
                             stringequal(cast a,cast b)
                          | 0x32 (* tag_word8array *) => false
                          | 0x36 (* tag_realdarray *) => false
                          | _ (* tagless pair *) => pairEq()
                        (* end case *)
                    end)
    end (* local *)

    val profile_sregister = ref(fn (x:Assembly.object,s:string)=>x)

  end

(*
 * $Log: core.sml,v $
 * Revision 1.6  1998/02/12 18:56:34  jhr
 *   Removed references to System.Tags.
 *
 * Revision 1.5  1998/02/04 22:23:02  jhr
 *   Merging in WalidTaha's changes for LazySML.
 *
 * Revision 1.4  1997/12/03 21:17:13  dbm
 *   Name changes: bytearray -> word8array; realarray -> real64array.
 *
 * Revision 1.3  1997/05/05  19:59:58  george
 *   Change the term language into the quasi-A-normal form. Added a new round
 *   of lambda contraction before and after type specialization and
 *   representation analysis. Type specialization including minimum type
 *   derivation is now turned on all the time. Real array is now implemented
 *   as realArray. A more sophisticated partial boxing scheme is added and
 *   used as the default.
 *
 * Revision 1.2  1997/02/11  15:16:06  george
 * moved stuff from System to SMLofNJ
 *
 * Revision 1.1.1.1  1997/01/14  01:38:14  george
 *   Version 109.24
 *
 *)
