(* built-in.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Interfaces to the compiler built-ins, infixes, etc.
 *
 *)

(* [dbm, 6/21/06] This module is compiled in the environment PrimEnv.primEnv.
   See init.cmi *)

structure PrimTypes = struct open PrimTypes end
   (* this silliness is to prevent elabstr.sml from sticking a NO_ACCESS
      in the wrong place [dbm: presumably this NO_ACCESS is derived from
      the dummy access value (NO_ACCESS) in the hand-built PrimTypes module.]
      How and why does this access value get propagated into the code. *)

local
    open PrimTypes
in
(* [dbm, 6/21/06] If this is elaborated in the primEnv environment, there is
already an opened PrimType, so is the above code redundnat? By experimentation,
it appears that the "local open PrimTypes in ..." is not necessary. *)

  structure Assembly = Core.Assembly

  (* The following code was used to create a type-safe version of the InLine
   * structure while preserving the inline property of the functions.
   * Since everything in InLine is now properly typed already, the code
   * should now be seen as:
   *   - organizing things a bit better
   *   - confirming the type information
   * See compiler/Semant/statenv/prim.sml for the origin of the type info
   * in InLine.    (Blume, 1/2001) *)
  structure InlineT =
   struct
    type 'a control_cont = 'a control_cont

    val callcc		: ('a cont -> 'a) -> 'a = InLine.callcc
    val throw	 	: 'a cont -> 'a -> 'b = InLine.throw
    val capture		: ('a control_cont -> 'a) -> 'a = InLine.capture
    val escape		: 'a control_cont -> 'a -> 'b = InLine.cthrow   
    val isolate         : ('a -> unit) -> 'a cont = InLine.isolate
    val !	 	: 'a ref -> 'a = InLine.!
    val op := 		: 'a ref * 'a -> unit = InLine.:=
    val makeref 	: 'a -> 'a ref = InLine.makeref
    val op = 		: ''a * ''a -> bool  = InLine.=
    val op <> 		: ''a * ''a -> bool = InLine.<>
    val boxed 		: 'a -> bool = InLine.boxed
    val unboxed 	: 'a -> bool = InLine.unboxed
    val cast 		: 'a -> 'b = InLine.cast
    val identity	: 'a -> 'a = InLine.cast
    val objlength	: 'a -> int = InLine.objlength
    val mkspecial	: int * 'a -> 'b = InLine.mkspecial
    val getspecial	: 'a -> int = InLine.getspecial
    val setspecial	: ('a * int) -> unit = InLine.setspecial
    val getpseudo	: int -> 'a = InLine.getpseudo 
    val setpseudo	: 'a * int -> unit = InLine.setpseudo 
    val gethdlr 	: unit -> 'a cont = InLine.gethdlr
    val sethdlr 	: 'a cont -> unit = InLine.sethdlr
    val getvar		: unit -> 'a = InLine.getvar
    val setvar		: 'a -> unit = InLine.setvar
    val compose 	: ('b -> 'c) * ('a -> 'b) -> ('a -> 'c) = InLine.compose
    val op before	: ('a * 'b) -> 'a = InLine.before
    val ignore          : 'a -> unit = InLine.ignore
    val gettag		: 'a -> int = InLine.gettag
    val setmark 	: 'a -> unit = InLine.setmark 
    val dispose 	: 'a -> unit = InLine.dispose 
    val inlnot		: bool -> bool = InLine.inlnot
    val recordSub	: ('a * int) -> 'b = InLine.recordSub
    val raw64Sub	: ('a * int) -> real = InLine.raw64Sub

    val ptreql          : 'a * 'a -> bool = InLine.ptreql

    structure Real64 =
      struct
        val op +   : real * real -> real = InLine.f64add
        val op -   : real * real -> real = InLine.f64sub
        val op /   : real * real -> real = InLine.f64div
        val op *   : real * real -> real = InLine.f64mul
        val op ==   : real * real -> bool = InLine.f64eq
        val op !=  : real * real -> bool = InLine.f64ne
        val op >=  : real * real -> bool = InLine.f64ge
        val op >   : real * real -> bool = InLine.f64gt
        val op <=  : real * real -> bool = InLine.f64le
        val op <   : real * real -> bool = InLine.f64lt
        val ~      : real -> real = InLine.f64neg
        val abs    : real -> real = InLine.f64abs

        val min    : real * real -> real  = InLine.f64min
        val max    : real * real -> real  = InLine.f64max

	val from_int31 : int -> real      = InLine.real
	val from_int32 : int32 -> real    = InLine.real32
	
	val signBit : real -> bool = InLine.f64sgn 
      end

    structure IntInf =
      struct
	val test_int31    : intinf -> int    = InLine.test_inf_31
        val test_int32    : intinf -> int32  = InLine.test_inf_32
	val trunc_word8   : intinf -> word8  = InLine.trunc_inf_8
	val trunc_word31  : intinf -> word   = InLine.trunc_inf_31
	val trunc_word32  : intinf -> word32 = InLine.trunc_inf_32
	val copy_word8    : word8 -> intinf  = InLine.copy_8_inf
	val copy_int31    : int -> intinf    = InLine.copy_31_inf_i
	val copy_word31   : word -> intinf   = InLine.copy_31_inf_w
	val copy_int32    : int32 -> intinf  = InLine.copy_32_inf_i
	val copy_word32   : word32 -> intinf = InLine.copy_32_inf_w
	val extend_word8  : word8 -> intinf  = InLine.extend_8_inf
	val extend_int31  : int -> intinf    = InLine.extend_31_inf_i
	val extend_word31 : word -> intinf   = InLine.extend_31_inf_w
	val extend_int32  : int32 -> intinf  = InLine.extend_32_inf_i
	val extend_word32 : word32 -> intinf = InLine.extend_32_inf_w

	val toInt = test_int31
	val fromInt = extend_int31
	val toLarge       : intinf -> intinf = InLine.identity
	val fromLarge     : intinf -> intinf = InLine.identity
      end

    structure Word32 =
      struct
        val test_int31   : word32 -> int   = InLine.test_32_31_w
	val testu_int31  : word32 -> int   = InLine.testu_32_31
	val testu_int32  : word32 -> int32 = InLine.testu_32_32
	val trunc_word31 : word32 -> word  = InLine.trunc_32_31_w
	val trunc_word8  : word32 -> word8 = InLine.trunc_32_8_w
	val copy_word8   : word8 -> word32 = InLine.copy_8_32_w
	val copy_word31  : word -> word32  = InLine.copy_31_32_w
	val copyf_int32  : int32 -> word32 = InLine.copy_32_32_iw
	val copyt_int32  : word32 -> int32 = InLine.copy_32_32_wi
	val copy_word32  : word32 -> word32 = InLine.copy_32_32_ww
	val extend_word8 : word8 -> word32 = InLine.extend_8_32_w
	val extend_int31 : int -> word32   = InLine.extend_31_32_iw
	val extend_word31: word -> word32  = InLine.extend_31_32_ww

	val toLargeWord   = copy_word32
	val toLargeWordX  = copy_word32
	val fromLargeWord = copy_word32
	val toLargeInt    = IntInf.copy_word32
	val toLargeIntX   = IntInf.extend_word32
	val fromLargeInt  = IntInf.trunc_word32
	val toInt         = testu_int31
	val toIntX        = test_int31
	val fromInt       = extend_int31

        val orb     : word32 * word32 -> word32 = InLine.w32orb
        val xorb    : word32 * word32 -> word32 = InLine.w32xorb
        val andb    : word32 * word32 -> word32 = InLine.w32andb
        val op *    : word32 * word32 -> word32 = InLine.w32mul
        val op +    : word32 * word32 -> word32 = InLine.w32add
        val op -    : word32 * word32 -> word32 = InLine.w32sub
	val ~       : word32 -> word32          = InLine.w32neg
        val op div  : word32 * word32 -> word32 = InLine.w32div
        val op mod  : word32 * word32 -> word32 = InLine.w32mod
        val op >    : word32 * word32 -> bool   = InLine.w32gt
        val op >=   : word32 * word32 -> bool   = InLine.w32ge
        val op <    : word32 * word32 -> bool   = InLine.w32lt
        val op <=   : word32 * word32 -> bool   = InLine.w32le
        val rshift  : word32 * word -> word32 = InLine.w32rshift
        val rshiftl : word32 * word -> word32 = InLine.w32rshiftl
        val lshift  : word32 * word -> word32 = InLine.w32lshift
        val notb    : word32 -> word32 = InLine.w32notb
	val chkLshift  : word32 * word -> word32 = InLine.w32ChkLshift
	val chkRshift  : word32 * word -> word32 = InLine.w32ChkRshift
	val chkRshiftl : word32 * word -> word32 = InLine.w32ChkRshiftl

        val min     : word32 * word32 -> word32  = InLine.w32min
        val max     : word32 * word32 -> word32  = InLine.w32max
      end

    structure Word64 =
      struct
        val extern : word64 -> word32 * word32 = InLine.w64p
	val intern : word32 * word32 -> word64 = InLine.p64w
      end

    structure Int32 =
      struct
        val test_int31   : int32 -> int    = InLine.test_32_31_i
	val trunc_word8  : int32 -> word8  = InLine.trunc_32_8_i
	val trunc_word31 : int32 -> word   = InLine.trunc_32_31_i
        val copy_word8   : word8 -> int32  = InLine.copy_8_32_i
	val copy_word31  : word -> int32   = InLine.copy_31_32_i
        val copy_int32   : int32 -> int32  = InLine.copy_32_32_ii
	val extend_word8 : word8 -> int32  = InLine.extend_8_32_i
	val extend_int31 : int -> int32    = InLine.extend_31_32_ii
	val extend_word31: word -> int32   = InLine.extend_31_32_wi

	val toInt = test_int31
	val fromInt = extend_int31
	val toLarge = IntInf.extend_int32
	val fromLarge = IntInf.test_int32

        val op *    : int32 * int32 -> int32  = InLine.i32mul
        val op quot : int32 * int32 -> int32  = InLine.i32quot  
        val op rem  : int32 * int32 -> int32  = InLine.i32rem  
        val op div  : int32 * int32 -> int32  = InLine.i32div  
        val op mod  : int32 * int32 -> int32  = InLine.i32mod  
        val op +    : int32 * int32 -> int32  = InLine.i32add  
        val op -    : int32 * int32 -> int32  = InLine.i32sub
        val ~       : int32 -> int32 = InLine.i32neg 
        val andb    : int32 * int32 -> int32  = InLine.i32andb
        val orb     : int32 * int32 -> int32  = InLine.i32orb
        val xorb    : int32 * int32 -> int32  = InLine.i32xorb
        val rshift  : int32 * int32 -> int32  = InLine.i32rshift
        val lshift  : int32 * int32 -> int32  = InLine.i32lshift
        val op <    : int32 * int32 -> bool = InLine.i32lt
        val op <=   : int32 * int32 -> bool = InLine.i32le
        val op >    : int32 * int32 -> bool = InLine.i32gt
        val op >=   : int32 * int32 -> bool = InLine.i32ge
        val op =    : int32 * int32 -> bool = InLine.i32eq
        val op <>   : int32 * int32 -> bool = InLine.i32ne

        val min     : int32 * int32 -> int32  = InLine.i32min
        val max     : int32 * int32 -> int32  = InLine.i32max
        val abs     : int32 -> int32 = InLine.i32abs
      end

    structure Word31 =
      struct
        val testu_int31   : word -> int    = InLine.testu_31_31
	val copyt_int31   : word -> int    = InLine.copy_31_31_wi
	val copyf_int31   : int -> word    = InLine.copy_31_31_iw

	val toLargeWord   = Word32.copy_word31
	val toLargeWordX  = Word32.extend_word31
	val fromLargeWord = Word32.trunc_word31
	val toLargeInt    = IntInf.copy_word31
	val toLargeIntX   = IntInf.extend_word31
	val fromLargeInt  = IntInf.trunc_word31
	val toInt         = testu_int31
	val toIntX        = copyt_int31
	val fromInt       = copyf_int31

        val orb     : word * word -> word = InLine.w31orb
        val xorb    : word * word -> word = InLine.w31xorb
        val andb    : word * word -> word = InLine.w31andb
        val op *    : word * word -> word = InLine.w31mul
        val op +    : word * word -> word = InLine.w31add
        val op -    : word * word -> word = InLine.w31sub
	val ~       : word -> word        = InLine.w31neg
        val op div  : word * word -> word = InLine.w31div
        val op mod  : word * word -> word = InLine.w31mod
        val op >    : word * word -> bool   = InLine.w31gt
        val op >=   : word * word -> bool   = InLine.w31ge
        val op <    : word * word -> bool   = InLine.w31lt
        val op <=   : word * word -> bool   = InLine.w31le
        val rshift  : word * word -> word = InLine.w31rshift
        val rshiftl : word * word -> word = InLine.w31rshiftl
        val lshift  : word * word -> word = InLine.w31lshift
	val chkLshift  : word * word -> word = InLine.w31ChkLshift
	val chkRshift  : word * word -> word = InLine.w31ChkRshift
	val chkRshiftl : word * word -> word = InLine.w31ChkRshiftl
        val notb    : word -> word = InLine.w31notb

        val min     : word * word -> word  = InLine.w31min
        val max     : word * word -> word  = InLine.w31max
      end

    structure Int31 =
      struct
	val trunc_word8 : int -> word8   = InLine.trunc_31_8
	val copy_int31   : int -> int    = InLine.copy_31_31_ii
	val copy_word8   : word8 -> int  = InLine.copy_8_31
	val extend_word8 : word8 -> int  = InLine.extend_8_31

	val toInt   = copy_int31
	val fromInt = copy_int31
	val toLarge = IntInf.extend_int31
	val fromLarge = IntInf.test_int31

        val op *    : int * int -> int  = InLine.i31mul
        val op quot : int * int -> int  = InLine.i31quot
        val op rem  : int * int -> int  = InLine.i31rem
        val op div  : int * int -> int  = InLine.i31div
        val op mod  : int * int -> int  = InLine.i31mod
        val op +    : int * int -> int  = InLine.i31add
        val op -    : int * int -> int  = InLine.i31sub
        val ~       : int -> int = InLine.i31neg
        val andb    : int * int -> int  = InLine.i31andb
        val orb     : int * int -> int  = InLine.i31orb
        val xorb    : int * int -> int  = InLine.i31xorb
        val rshift  : int * int -> int  = InLine.i31rshift
        val lshift  : int * int -> int  = InLine.i31lshift
        val notb    : int -> int = InLine.i31notb
        val op <    : int * int -> bool = InLine.i31lt
        val op <=   : int * int -> bool = InLine.i31le
        val op >    : int * int -> bool = InLine.i31gt
        val op >=   : int * int -> bool = InLine.i31ge
        val op =    : int * int -> bool = InLine.i31eq
        val op <>   : int * int -> bool = InLine.i31ne
        val ltu     : int * int -> bool = InLine.i31ltu
        val geu     : int * int -> bool = InLine.i31geu
  
        val min     : int * int -> int  = InLine.i31min
        val max     : int * int -> int  = InLine.i31max
        val abs     : int -> int = InLine.i31abs
      end

    structure Int64 =
      struct
        val extern : int64 -> word32 * word32 = InLine.i64p
	val intern : word32 * word32 -> int64 = InLine.p64i
      end

    structure Word8 =
      struct

        (* LargeInt is still 32 bit: *)
        val toLargeWord   = Word32.copy_word8
	val toLargeWordX  = Word32.extend_word8
	val fromLargeWord = Word32.trunc_word8
	val toLargeInt    = IntInf.copy_word8
	val toLargeIntX   = IntInf.extend_word8
	val fromLargeInt  = IntInf.trunc_word8
	val toInt         = Int31.copy_word8
	val toIntX        = Int31.extend_word8
	val fromInt       = Int31.trunc_word8

      (* temporary framework, because the actual word8 operators 
       * are not implemented*)
	(* WARNING! some of the following operators
	 *          don't get the high-order bits right *)
        val orb     : word8 * word8 -> word8 = InLine.w31orb_8
        val xorb    : word8 * word8 -> word8 = InLine.w31xorb_8
        val andb    : word8 * word8 -> word8 = InLine.w31andb_8
        val op *    : word8 * word8 -> word8 = InLine.w31mul_8
        val op +    : word8 * word8 -> word8 = InLine.w31add_8
        val op -    : word8 * word8 -> word8 = InLine.w31sub_8
	val ~       : word8 -> word8         = InLine.w31neg_8
        val op div  : word8 * word8 -> word8 = InLine.w31div_8
        val op mod  : word8 * word8 -> word8 = InLine.w31mod_8
        val op >    : word8 * word8 -> bool  = InLine.w31gt_8
        val op >=   : word8 * word8 -> bool  = InLine.w31ge_8
        val op <    : word8 * word8 -> bool  = InLine.w31lt_8
        val op <=   : word8 * word8 -> bool  = InLine.w31le_8
        val rshift  : word8 * word -> word8  = InLine.w31rshift_8
        val rshiftl : word8 * word -> word8  = InLine.w31rshift_8
        val lshift  : word8 * word -> word8  = InLine.w31lshift_8
        val notb    : word8 -> word8         = InLine.w31notb_8
	val chkLshift  : word8 * word -> word8 = InLine.w31ChkLshift_8
	val chkRshift  : word8 * word -> word8 = InLine.w31ChkRshift_8
	val chkRshiftl : word8 * word -> word8 = InLine.w31ChkRshiftl_8

	val min     : word8 * word8 -> word8 = InLine.w31min_8
	val max     : word8 * word8 -> word8 = InLine.w31max_8
        end

    structure Char =
      struct

        val maxOrd = 255
        exception Chr

      (* the following should be an inline operator *)
        fun chr i = if (Int31.geu(i, Int31.+(maxOrd,1)))
	    then raise Chr
	    else ((InLine.cast i) : char)

        val ord : char -> int = InLine.cast

        val (op <)  : (char * char) -> bool = InLine.i31lt_c
        val (op <=) : (char * char) -> bool = InLine.i31le_c
        val (op >)  : (char * char) -> bool = InLine.i31gt_c
        val (op >=) : (char * char) -> bool = InLine.i31ge_c
      end

    structure PolyArray =
      struct
 	val newArray0 : unit -> 'a array = InLine.newArray0
        val array     : int * 'a -> 'a array = InLine.mkarray 
        val length    : 'a array -> int = InLine.length
        val sub       : 'a array * int -> 'a = InLine.arrSub
        val chkSub    : 'a array * int -> 'a = InLine.arrChkSub
        val update    : 'a array * int * 'a -> unit = InLine.arrUpdate
        val chkUpdate : 'a array * int * 'a -> unit = InLine.arrChkUpdate
	val getData   : 'a array -> 'b = InLine.getSeqData
      end

    structure PolyVector =
      struct
        val length    : 'a vector -> int = InLine.length 
        val sub       : 'a vector * int -> 'a = InLine.vecSub
        val chkSub    : 'a vector * int -> 'a = InLine.vecChkSub
	val getData   : 'a vector -> 'b = InLine.getSeqData
      end

  (* The type of this ought to be float64array *)
    structure Real64Array =
      struct
 	val newArray0 : unit -> Assembly.A.real64array = InLine.newArray0
        val length    : Assembly.A.real64array -> int = InLine.length
        val sub       : Assembly.A.real64array * int -> real = InLine.f64Sub
        val chkSub    : Assembly.A.real64array * int -> real = InLine.f64chkSub
        val update    : Assembly.A.real64array * int * real -> unit = InLine.f64Update
        val chkUpdate : Assembly.A.real64array * int * real -> unit = InLine.f64chkUpdate
	val getData   : Assembly.A.real64array -> 'b = InLine.getSeqData
      end

  (** NOTE: we are currently using polymorphic vectors to implement the Real64Vector
   ** structure.
   **)
    structure Real64Vector =
      struct
        val length    : real vector -> int = InLine.length 
        val sub       : real vector * int -> real = InLine.vecSub
        val chkSub    : real vector * int -> real = InLine.vecChkSub
	val getData   : real vector -> 'b = InLine.getSeqData
      end

    structure Word8Array =
      struct
	type array = Assembly.A.word8array
 	val newArray0 : unit -> array = InLine.newArray0
        val length    : array -> int = InLine.length
    (* BUG: using "ordof" for W8A.sub is dangerous, because ordof is
     (technically) fetching from immutable things.  A fancy optimizer might
     someday be confused. *)
        val sub       : array * int -> word8 = InLine.ordof
        val chkSub    : array * int -> word8 = InLine.inlbyteof
        val update    : array * int * word8 -> unit = InLine.store
        val chkUpdate : array * int * word8 -> unit = InLine.inlstore
	val getData   : array -> 'a = InLine.getSeqData
      end

    (* preliminary version with just the type *)
    structure Word8Vector :> sig
	eqtype vector
        val create : int -> vector
    end = struct
        type vector = string
	val create = Assembly.A.create_s
    end

    (* now the real version with all values *)
    structure Word8Vector =
      struct
        open Word8Vector
	val length    : vector -> int = InLine.length
        val sub       : vector * int -> word8 = InLine.ordof
        val chkSub    : vector * int -> word8 = InLine.inlordof
        val update    : vector * int * word8 -> unit = InLine.store
	val getData   : vector -> 'a = InLine.getSeqData
      end

    structure CharArray :> sig		(* prelim *)
	eqtype array
        val newArray0 : unit -> array
	val create : int -> array
    end = struct
        type array = Assembly.A.word8array
	val newArray0 : unit -> array = InLine.newArray0
	val create = Assembly.A.create_b
    end

    structure CharArray =		(* full *)
      struct
	open CharArray
	val length    : array -> int = InLine.length
	val chkSub    : (array * int) -> char = InLine.inlordof
	val chkUpdate : (array * int * char) -> unit = InLine.inlstore
	val sub       : (array * int) -> char = InLine.ordof
	val update    : (array * int * char) -> unit = InLine.store
	val getData   : array -> 'a = InLine.getSeqData
      end

    structure CharVector =
      struct
	val length    : string -> int			= InLine.length
	val chkSub    : (string * int) -> char		= InLine.inlordof
	val sub       : (string * int) -> char		= InLine.ordof
	val update    : (string * int * char) -> unit	= InLine.store
	val getData   : string -> 'a = InLine.getSeqData
      end

    structure DfltInt  = Int31
    structure DfltWord  = Word31
    structure DfltReal = Real64

   end  (* structure InlineT *)

end (* local *)
