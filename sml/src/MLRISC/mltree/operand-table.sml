(*
 * A table for storing operands for a compilation unit.
 * We give each distinct operand a unique (negative) value number.
 *)
functor OperandTable(Props : INSN_PROPERTIES) : OPERAND_TABLE =
struct

   structure I  = Props.I
   structure IH = IntHashTable
   structure H  = HashTable
       
   type value = int

   datatype const =
     INT     of int           (* small integer operands *)
   | INTINF  of IntInf.int    (* large integer operands *)
   | OPERAND of I.operand     (* other operand *)

   datatype operandTable =
      TABLE of 
      {  intTable   : value IH.hash_table,
         miTable    : (IntInf.int,value) H.hash_table,
         opnTable   : (I.operand,value) H.hash_table,
         constTable : const IH.hash_table, (*value number -> const*)
         nextValueNumber : value ref
      }

   type valueNumber =
      { int     : int -> value,
        word    : word -> value,
        int32   : Int32.int -> value,
        word32  : Word32.word -> value,
        intinf  : IntInf.int -> value,
        operand : I.operand -> value
      }

   exception NoOperand
   exception NoConst
   exception NoInt
   exception NoIntInf

   val two_to_the_31 = IntInf.pow(IntInf.fromInt 2,31)
   fun hashIntInf i = Word.fromInt(IntInf.toInt(IntInf.rem(i,two_to_the_31)))

   fun create(nextValueNumber) =
   let val constTable = IH.mkTable (37,NoConst)
       val opnTable   = H.mkTable(Props.hashOpn,Props.eqOpn) (32,NoOperand)
       val intTable   = IH.mkTable (32, NoInt)
       val miTable    = H.mkTable(hashIntInf,op =) (7, NoIntInf)

       fun newInt i =
       let val v = !nextValueNumber (* value number *)
       in  nextValueNumber := v - 1;
           IH.insert intTable (i,v);
           IH.insert constTable (v, INT i);
           v
       end

       fun init(n,0) = ()
         | init(n,m) = (newInt n; init(n+1,m-1))

   in  init(0,2);
       TABLE{ intTable        = intTable,
              miTable         = miTable,
              opnTable        = opnTable,
              constTable      = constTable,
              nextValueNumber = nextValueNumber
            }
   end

   fun wordToIntInf w = IntInf.fromInt(Word.toIntX w)
   fun word32ToIntInf w = IntInf.fromLarge(Word32.toLargeIntX w)
   fun wordToInt w = Word.toIntX w
   fun word32ToInt w = Word32.toIntX w
   fun intInfToInt i = IntInf.toInt i
   fun intInfToInt32 i = IntInf.toLarge i
   fun intToIntInf i  = IntInf.fromInt i
   fun intToInt32 i   = Int32.fromInt i
   fun int32ToIntInf i = IntInf.fromLarge i  
   fun int32ToInt i = Int32.toInt i
   
   (* Lookup the value number of a constant *)
   fun int(TABLE{intTable, ...}) = IH.lookup intTable  

   fun word(TABLE{intTable, ...}) w = IH.lookup intTable (wordToInt w)

   fun word32(TABLE{intTable, miTable, ...}) w = 
         IH.lookup intTable (word32ToInt w) handle Overflow =>
          H.lookup miTable (word32ToIntInf w)

   fun int32(TABLE{intTable, miTable, ...}) w = 
         IH.lookup intTable (int32ToInt w) handle Overflow =>
          H.lookup miTable (int32ToIntInf w)

   fun intinf(TABLE{intTable, miTable, ...}) i = 
         IH.lookup intTable (intInfToInt i) handle Overflow =>
          H.lookup miTable i

   fun operand(TABLE{opnTable,...}) = H.lookup opnTable

   fun lookupValueNumbers tbl =
       { int = int tbl,
         word = word tbl,
         word32 = word32 tbl,
         int32 = int32 tbl,
         intinf = intinf tbl,
         operand = operand tbl
       }

   (* create new value numebers *)
   fun makeNewValueNumbers(TABLE{opnTable,constTable,
                                 nextValueNumber,intTable,miTable,...}) =
   let val findOpn = H.find opnTable
       val findInt = IH.find intTable
       val findIntInf = H.find miTable 
       val insertOpn = H.insert opnTable
       val insertInt = IH.insert intTable
       val insertIntInf = H.insert miTable
       val insertConst = IH.insert constTable

       fun newConst(const) = 
       let val v = !nextValueNumber
           val _ = nextValueNumber := v - 1
       in  insertConst (v,const); v
       end

       fun mkOpn opn = 
           case findOpn opn of
             SOME v => v 
           | NONE => let val v = newConst(OPERAND opn)
                     in  insertOpn(opn, v); v end
       fun mkInt i =
           case findInt i of
             SOME v => v
           | NONE => let val v = newConst(INT i)
                     in  insertInt(i, v); v end

       fun mkIntInf' i =
           case findIntInf i of
             SOME v => v
           | NONE => let val v = newConst(INTINF i)
                     in  insertIntInf(i, v); v end

       fun mkIntInf i = mkInt(intInfToInt i) handle _ => mkIntInf' i

       fun mkWord w = mkInt(wordToInt w)

       fun mkInt32 i = mkInt(int32ToInt i)
                        handle _ => mkIntInf'(int32ToIntInf i)

       fun mkWord32 w = mkInt(word32ToInt w)
                        handle _ => mkIntInf'(word32ToIntInf w)
   in  {int=mkInt,
        word=mkWord,
        word32=mkWord32,
        int32=mkInt32,
        intinf=mkIntInf,
        operand=mkOpn
       }
   end

   (* value number -> const *)
   fun const(TABLE{constTable,...}) = IH.lookup constTable

end
