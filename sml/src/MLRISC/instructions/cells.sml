(*
 * Description of cell and other updatable cells.
 * 
 * -- Allen.
 *) 
functor CellsBasisFn
   (eqtype cellkind
    exception Cells
    val unknown     : cellkind
    val cellkindToString : cellkind -> string
    val INT         : cellkind
    val FLOAT       : cellkind
    val firstPseudo : int
    val kinds       : cellkind list
    val physical    : {kind:cellkind,from:int,to:int} list
   ) : CELLS_BASIS = 
struct
   type cellkind = cellkind
   type cell = int
   type regmap = cell Intmap.intmap
   exception Cells = Cells

   val cellkinds = kinds
   val cellkindToString = cellkindToString

   val initSize = firstPseudo * 4 
   val cellKindTable = Intmap.new(initSize,Cells) : cellkind Intmap.intmap
   val lookupCellKind = Intmap.map cellKindTable
   val cellKind = Intmap.mapWithDefault (cellKindTable,INT)
   val updateCellKind = Intmap.add  cellKindTable

   val cellKindOn = MLRiscControl.getFlag "register-cellkind";

   fun init() =
      if !cellKindOn then
      (Intmap.clear(cellKindTable);
       app (fn {kind,from,to} =>
               let fun loop r = 
                       if r > to then () else
                       (Intmap.add cellKindTable (r,kind); loop(r+1))
               in  loop from end
           ) physical
      ) else ()

   val firstPseudo = firstPseudo

   val name = ref firstPseudo
   val counters = map (fn _ => ref 0) kinds

   fun lookupCnt c =
       let fun f(c'::cs,r::rs) = if c = c' then r else f(cs,rs)
             | f _ = (print("lookupCnt "^cellkindToString c^"\n"); raise Cells)
       in  f(kinds,counters) end

   fun cellRange k =
   let fun find({kind,from,to}::rest) = 
            if kind = k then {low=from,high=to}
            else find rest
         | find [] = raise Cells
   in  find physical end

   fun Reg k =
   let val {high,low} = cellRange k
       val count = high-low+1
   in  fn nth => if 0 <= nth andalso nth < count then nth+low
                 else raise Cells
   end

   val GPReg = Reg INT
   val FPReg = Reg FLOAT

   fun newCell c = 
       let val cnt = lookupCnt c
       in  fn _ => 
           let val r = !name 
           in  name := r + 1; 
               cnt := !cnt + 1;
               if !cellKindOn andalso c <> INT 
               then updateCellKind (r,c) else ();
               r 
           end
       end

   local val cnt = lookupCnt INT
   in fun newReg _ = 
      let val r = !name 
      in  name := r + 1; 
          cnt := !cnt + 1;
          r 
      end
   end

   local val cnt = lookupCnt FLOAT
   in fun newFreg _ = 
      let val r = !name 
      in  name := r + 1; 
          cnt := !cnt + 1;
          if !cellKindOn then updateCellKind (r,FLOAT) else ();
          r 
      end
   end

   fun newVar r' =
   let val r = !name
   in  name := r + 1;
       if !cellKindOn then
         (updateCellKind(r,lookupCellKind r') handle _ => ())
       else ();
       r    
   end

   fun numCell c = let val cnt = lookupCnt c in fn () => !cnt end

   fun maxCell() = !name

   fun regmap() = 
   let val map = Intmap.new(32,Cells)
       val add = Intmap.add map
       (* initialize the regmap with physical register bindings *)
       fun init [] = ()
         | init({from,to,kind}::rest) = (ins(from,to); init rest)
       and ins(r,limit) = if r > limit then () else (add(r,r); ins(r+1,limit))
   in  init physical;
       map
   end

   val lookup = Intmap.mapInt 

   fun reset() = (init();
                  app (fn r => r := 0) counters;
                  name := firstPseudo
                 )

   fun printSet f set = 
   let fun g []     = ["}"]
         | g [x]    = [f x,"}"]
         | g (x::y) = f x::" "::g y
   in  String.concat("{"::g set) end

   fun printTuple(c::cs,s::sets) = c^"="^s^"  "^printTuple(cs,sets)
     | printTuple _ = ""
end
