(*
 * A table for storing operands for a compilation unit.
 * We give each distinct operand a unique (negative) value number.
 *)
functor OperandTable(Props : INSN_PROPERTIES) : OPERAND_TABLE =
struct

   structure I  = Props.I
   structure HA = HashArray
   structure HT = HashTable
       
   type value = int

   datatype const =
     IMMED of int           (* integer operand *)
   | OPERAND of I.operand   (* other operand *)
   | LABEL of Label.label   (* a label *)

   datatype operandTable =
      TABLE of 
      {  immTable   : value HA.array,
         labelTable : (Label.label,value) HT.hash_table,
         opnTable   : (I.operand,value) HT.hash_table,
         constTable : const HA.array,
         nextImmed  : int ref
      }
 
   exception NoLabel
   exception NoOperand
   exception NoConst

   fun create(nextImmed) =
   let val constTable = HA.array'(37, fn _ => raise NoConst)
       fun newImmed i =
       let val v = !nextImmed
       in  nextImmed := v - 1;
           HA.update(constTable, v, IMMED i); 
           v
       end
       fun hashLabel(Label.Label{id,...}) = Word.fromInt id
       fun eqLabel(Label.Label{id=x,...},Label.Label{id=y,...}) = x=y
       val immTable   = HA.array''(37,newImmed)
       val opnTable   = HT.mkTable(Props.hashOpn,Props.eqOpn) (32,NoOperand)
       val labelTable = HT.mkTable(hashLabel,eqLabel) (32,NoLabel)
   in  TABLE{ immTable   = immTable,
              labelTable = labelTable,
              opnTable   = opnTable,
              constTable = constTable,
              nextImmed  = nextImmed
            }
   end
 

   (* Lookup/insert an immediate constant *)
   fun immed(TABLE{immTable, ...}) i = HA.sub(immTable,i)

   (* lookup/insert a label *)
   fun label(TABLE{labelTable,constTable,nextImmed,...}) =
   let val look = HT.lookup labelTable
       val ins  = HT.insert labelTable
       fun lookup l = look l handle NoLabel =>
           let val v = !nextImmed
               val _ = nextImmed := v - 1
           in  HA.update(constTable,v,LABEL l); ins(l,v); v end
   in  lookup end

   (* lookup/insert an operand *)
   fun operand(TABLE{opnTable,constTable,nextImmed,...}) =
   let val look = HT.lookup opnTable
       val ins  = HT.insert opnTable
       fun lookup opn = look opn handle NoOperand =>
           let val v = !nextImmed
               val _ = nextImmed := v - 1
           in  HA.update(constTable,v,OPERAND opn); ins(opn,v); v end
   in  lookup end

   (* lookup a constant/operand *)
   fun const(TABLE{constTable,...}) v = HA.sub(constTable,v) 

end
