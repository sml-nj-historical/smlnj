(* built-in.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 * Interfaces to the compiler built-ins, infixes, etc.
 *
 *)

infix 7  * /  mod  div
infix 6 ^ + -
infix 3 := o
infix 4 > < >= <= = <>
infixr 5 :: @
infix 0 before

structure PrimTypes = struct open PrimTypes end
   (* this silliness is to prevent elabstr.sml from sticking a NO_ACCESS
       in the wrong place *)

open PrimTypes

structure Assembly = Core.Assembly

(* create a type-safe version of the InLine structure while preserving
 * the inline property of the functions.
 *)
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
    val gettag		: 'a -> int = InLine.gettag
    val setmark 	: 'a -> unit = InLine.setmark 
    val dispose 	: 'a -> unit = InLine.dispose 
    val inlnot		: bool -> bool = InLine.inlnot
    val real		: int -> real = InLine.real   
    val recordSub	: ('a * int) -> 'b = InLine.recordSub
    val raw64Sub	: ('a * int) -> real = InLine.raw64Sub

    structure Int31 =
      struct
	val fromInt : int -> int  = InLine.copy_31_31
	val toInt   : int -> int  = InLine.copy_31_31
        val op *    : int * int -> int  = InLine.i31mul
        val op quot : int * int -> int  = InLine.i31quot
        val op rem  : int * int -> int  = InLine.i31rem
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
  
        val op mod  : int * int -> int  = InLine.i31mod
        val op div  : int * int -> int  = InLine.i31div
        val min     : int * int -> int  = InLine.i31min
        val max     : int * int -> int  = InLine.i31max
        val abs     : int -> int = InLine.i31abs
      end

    structure Int32 =
      struct
	val toLarge : int32 -> int32 = InLine.copy_32_32
	val fromInt : int -> int32 = InLine.extend_31_32
	val toInt   : int32 -> int = InLine.test_32_31
	val fromLarge : int32 -> int32 = InLine.copy_32_32

        val op *    : int32 * int32 -> int32  = InLine.i32mul
        val op quot : int32 * int32 -> int32  = InLine.i32quot  
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
      end


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
      end

    structure Word32 =
      struct
	val toLargeInt : word32 -> int32 = InLine.testu_32_32 
	val toLargeIntX : word32 -> int32 = InLine.copy_32_32
	val fromLargeInt : int32 -> word32 = InLine.copy_32_32
	val fromInt : int -> word32 = InLine.extend_31_32
	val toLargeWord : word32 -> word32 = InLine.copy_32_32
	val toLargeWordX : word32 -> word32 = InLine.copy_32_32
	val fromLargeWord : word32 -> word32 = InLine.copy_32_32
	val toIntX : word32 -> int = InLine.test_32_31
	val toInt : word32 -> int = InLine.testu_32_31

        val orb     : word32 * word32 -> word32 = InLine.w32orb
        val xorb    : word32 * word32 -> word32 = InLine.w32xorb
        val andb    : word32 * word32 -> word32 = InLine.w32andb
        val op *    : word32 * word32 -> word32 = InLine.w32mul
        val op +    : word32 * word32 -> word32 = InLine.w32add
        val op -    : word32 * word32 -> word32 = InLine.w32sub
        val op div  : word32 * word32 -> word32 = InLine.w32div
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
      end

    structure Word31 =
      struct
	val toLargeInt    : word -> int32 = InLine.copy_31_32
	val toLargeIntX   : word -> int32 = InLine.extend_31_32
	val toInt         : word -> int = InLine.testu_31_31
	val toLargeWord   : word -> word32 = InLine.copy_31_32
	val toLargeWordX  : word -> word32 = InLine.extend_31_32
	val fromInt       : int -> word = InLine.copy_31_31
	val fromLargeWord : word32 -> word = InLine.trunc_32_31
	val fromLargeInt  : int32 -> word = InLine.trunc_32_31
	val toIntX        : word -> int = InLine.copy_31_31

        val orb     : word * word -> word = InLine.w31orb
        val xorb    : word * word -> word = InLine.w31xorb
        val andb    : word * word -> word = InLine.w31andb
        val op *    : word * word -> word = InLine.w31mul
        val op +    : word * word -> word = InLine.w31add
        val op -    : word * word -> word = InLine.w31sub
        val op div  : word * word -> word = InLine.w31div
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
      end

    structure Word8 =
      struct
	val toLargeIntX   : word8 -> int32  = InLine.extend_8_32
	val toLargeInt    : word8 -> int32  = InLine.copy_8_32
	val toIntX        : word8 -> int    = InLine.extend_8_31
        val toInt         : word8 -> int = InLine.copy_8_31
	val toLargeWordX  : word8 -> word32 = InLine.extend_8_32
	val toLargeWord   : word8 -> word32 = InLine.copy_8_32
	val fromLargeWord : word32 -> word8 = InLine.trunc_32_8
	val fromInt	  : int -> word8 = InLine.trunc_31_8
	val fromLargeInt  : int32 -> word8 = InLine.trunc_32_8

      (* temporary framework, because the actual word8 operators 
       * are not implemented*)
        val orb     : word8 * word8 -> word8 = InLine.i31orb
        val xorb    : word8 * word8 -> word8 = InLine.i31xorb
        val op div  : word8 * word8 -> word8 = InLine.i31div
        val andb    : word8 * word8 -> word8 = InLine.i31andb
        val op >    : word8 * word8 -> bool  = InLine.i31gt
        val op >=   : word8 * word8 -> bool  = InLine.i31ge
        val op <    : word8 * word8 -> bool  = InLine.i31lt
        val op <=   : word8 * word8 -> bool  = InLine.i31le
        val rshift  : word8 * word -> word8 = InLine.i31rshift
        val rshiftl : word8 * word -> word8 = InLine.i31rshift (* high bits always 0 *)
        val lshift  : word8 * word -> word8 = InLine.i31lshift
(* WARNING! the following operators don't get the high-order bits right *)
        val notb    : word8 -> word8 = InLine.i31notb  
        val op *    : word8 * word8 -> word8 = InLine.i31mul
        val op +    : word8 * word8 -> word8 = InLine.i31add
        val op -    : word8 * word8 -> word8 = InLine.i31sub
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

        val (op <)  : (char * char) -> bool = InLine.i31lt
        val (op <=) : (char * char) -> bool = InLine.i31le
        val (op >)  : (char * char) -> bool = InLine.i31gt
        val (op >=) : (char * char) -> bool = InLine.i31ge
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

    structure Word8Vector =
      struct
	local
	  structure V :> sig
	      eqtype vector
	      val create : int -> vector
	    end = struct
	      type vector = string
	      val create = Assembly.A.create_s
	    end
	in
	open V
	end
        val length    : vector -> int = InLine.length
        val sub       : vector * int -> word8 = InLine.ordof
        val chkSub    : vector * int -> word8 = InLine.inlordof
        val update    : vector * int * word8 -> unit = InLine.store
	val getData   : vector -> 'a = InLine.getSeqData
      end

    structure CharArray =
      struct
	local
	  structure A :> sig
	      eqtype array
	      val newArray0 : unit -> array
	      val create : int -> array
	    end = struct
	      type array = Assembly.A.word8array
	      val newArray0 : unit -> array = InLine.newArray0
	      val create = Assembly.A.create_b
	    end
	in
	open A
	end
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

