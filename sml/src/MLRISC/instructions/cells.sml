(*
 * Description of cell and other updatable cells.
 * 
 * -- Allen.
 *) 

(*
 * This functor is applied to create the cells structure for an  architecture
 *)
functor CellsCommon
   (exception Cells
    val firstPseudo   : int
    val cellKindDescs : (CellsBasis.cellkind * CellsBasis.cellkindDesc) list
   ) : CELLS_COMMON = 
struct

   open CellsBasis CellsInternal

   structure CellsBasis = CellsBasis

   exception Cells = Cells

   val i2s = Int.toString

   fun error msg = MLRiscErrorMsg.error(exnName Cells, msg)
   (*
   val cellKindDescs =
       (CONST,
        DESC{high= ~1, low=0, physicalRegs=ref(CellsInternal.array0),
             kind=CONST, counter=ref 0, 
             toString=fn v => "v"^i2s v,
             toStringWithSize=fn (v,_) => "v"^i2s v,
             defaultValues=[], 
             zeroReg=NONE
            } 
       ) :: 
       cellKindDescs
    *)

   val cellkinds   = map (fn (kind,_) => kind) cellKindDescs
   val firstPseudo = firstPseudo
   val name        = ref firstPseudo
   val cellCounter = name

   val _ = app (fn (_, desc as DESC{physicalRegs, high, low, ...}) =>
                let val n = high - low + 1
                in  if n <= 0 then ()
                    else let val a = Array.tabulate(n, fn nth =>
                                   let val reg = nth + low
                                   in  CELL{id=reg, col=ref(MACHINE reg), 
                                       an=ref [], desc=desc} 
                                   end)
                         in  physicalRegs := a
                         end
                end) cellKindDescs

   fun nextName() = let val id = !name in name := !name + 1; id end

   fun desc(k:cellkind) =
   let fun loop [] = error("missing info for "^cellkindToString k)
         | loop((kind,info)::defs) =
           if kind = k then info else loop defs
   in  loop cellKindDescs end

   val cellkindDesc = desc

   fun cellRange k = 
   let val DESC{low,high,...} = desc k
   in  {low=low,high=high} end

   fun Reg k =
   let val desc as DESC{low,kind,physicalRegs,...} = desc k
   in  fn nth => Array.sub(!physicalRegs,nth) handle _ => raise Cells
   end

   fun Regs k =
   let val Reg = Reg k
       fun loop{from, to, step} =
           if from > to then []
           else Reg from :: loop{from=from+step, to=to, step=step}
   in  loop end  

   fun Cell k =
   let val desc as DESC{low,kind,physicalRegs,...} = desc k
   in  fn reg => 
           Array.sub(!physicalRegs,reg - low)  handle _ => raise Cells
   end

   val GPReg = Reg GP
   val FPReg = Reg FP

   (* Counters *)
   fun newCell k = 
       let val desc as DESC{counter,...} = desc k
       in  fn _ => 
           let val r = !name 
           in  name := r + 1; 
               counter := !counter + 1;
               CELL{id=r, col=ref PSEUDO, an=ref [], desc=desc}
           end
       end

   local val desc as DESC{counter, ...} = desc GP
   in fun newReg _ = 
      let val r = !name 
      in  name := r + 1; 
          counter := !counter + 1;
          CELL{id=r, col=ref PSEUDO, an=ref [], desc=desc}
      end
   end

   local val desc as DESC{counter, ...} = desc FP
   in fun newFreg _ = 
      let val r = !name 
      in  name := r + 1; 
          counter := !counter + 1;
          CELL{id=r, col=ref PSEUDO, an=ref [], desc=desc}
      end
   end

   fun newVar (CELL{desc, an, ...}) =
   let val r = !name
   in  name := r + 1; 
       CELL{id=r, col=ref PSEUDO, an=ref(!an), desc=desc}    
   end

   fun cloneCell c =
   let val CELL{desc, an, col, ...} = chase c
       val r = !name
   in  name := r + 1; 
       CELL{id=r, col=ref(!col), an=ref(!an), desc=desc}    
   end
 
   fun numCell k = let val DESC{counter, ...} = desc k 
                   in fn () => !counter end

   fun maxCell() = !name

   fun reset() = 
       (app (fn (_,DESC{counter, ...}) => counter := 0) cellKindDescs;
        name := firstPseudo
       )

   structure CellSet =
   struct
      type cellset = (cellkindDesc * cell list) list
      val empty = []

      fun same(DESC{counter=c1,...}, DESC{counter=c2,...}) = c1=c2

      fun descOf (CELL{desc, ...}) = desc 

      fun add (r, cellset:cellset) =
      let val k = descOf r
          fun loop [] = [(k,[r])]
            | loop((x as (k',s))::cellset) = 
               if same(k,k') then (k',r::s)::cellset 
               else x::loop cellset
      in  loop cellset end

      fun rmv (r, cellset:cellset) =
      let val k = descOf r
          val c = registerId r
          fun filter [] = []
            | filter(r::rs) = if registerId r = c then filter rs 
                              else r::filter rs
          fun loop [] = []
            | loop((x as (k',s))::cellset) = 
               if same(k,k') then (k',filter s)::cellset else x::loop cellset
      in  loop cellset end

      fun get' k (cellset:cellset) =
      let fun loop [] = []
            | loop((x as (k',s))::cellset) = 
               if same(k,k') then s else loop cellset
      in  loop cellset end

      fun get k = get'(desc k)

      fun update' k (cellset:cellset,s) =
      let fun loop [] = [(k,s)]
            | loop((x as (k',_))::cellset) = 
               if same(k,k') then (k',s)::cellset else x::loop cellset
      in  loop cellset end

      fun update k = update'(desc k)   

      fun map {from,to} (cellset:cellset) =
      let val CELL{desc=k,...} = from
          val cf = registerId from 
          fun trans r = if registerId r = cf then to else r
          fun loop [] = []
            | loop((x as (k',s))::cellset) = 
               if same(k,k') then (k',List.map trans s)::cellset 
               else x::loop cellset
      in  loop cellset end

      val toCellList : cellset -> cell list = 
          List.foldr (fn ((_,S),S') => S @ S') [] 

      (* Pretty print cellset *)
      fun printSet(f,set,S) =
      let fun loop([], S) = "}"::S
            | loop([x], S) = f(chase x)::"}"::S
            | loop(x::xs, S) = f(chase x)::" "::loop(xs, S)
      in  "{"::loop(set, S) end

      fun toString' cellset =
      let fun pr cellset = 
          let fun loop((DESC{kind, toString, ...},s)::rest, S)=
                  (case s of
                     [] => loop(rest, S)
                   | _  => cellkindToString kind::"="::
                           printSet(toString o registerId,s," "::loop(rest,S))
                  )
                | loop([],S) = S
          in  String.concat(loop(cellset, [])) 
          end
      in  pr cellset end

      val toString = toString'
   end

   type cellset = CellSet.cellset
   val empty   = CellSet.empty
   val getReg  = CellSet.get GP
   val getFreg = CellSet.get FP
   val addReg  = CellSet.add 
   val addFreg = CellSet.add 
   val rmvReg  = CellSet.rmv 
   val rmvFreg = CellSet.rmv 

  (* Misc *)
   fun zeroReg k = 
   let val desc as DESC{zeroReg, physicalRegs, low, ...} = desc k
   in  case zeroReg of 
         NONE => NONE
       | SOME r => SOME(Array.sub(!physicalRegs, r)) 
   end

   fun defaultValues k = 
   let val DESC{defaultValues, ...} = desc k
   in  defaultValues end 
end
