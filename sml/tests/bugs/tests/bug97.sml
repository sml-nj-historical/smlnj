(* bug 97, Tofte -- type error in last line *)

signature IntMapSig=
sig
  type 'a map
  exception MapFail
  val apply: 'a map * int -> 'a
  val update: 'a map * int * 'a -> 'a map
  val emptyMap: 'a map
end;

signature ValSig =
sig
  type value
end;

signature SymSig=
sig
  eqtype sym
  val hash: sym -> int
end;


functor SymTblFct(
  structure IntMap: IntMapSig
  structure Val: ValSig
  structure Sym: SymSig):

    sig
      type table
      exception Lookup
      val emptyTable: table
      val update: table * Sym.sym * Val.value -> table
    end=

struct
  datatype table = TBL of
   (Sym.sym * Val.value)list IntMap.map
  val emptyTable = TBL IntMap.emptyMap;

  exception Lookup
  
  fun update(TBL map,s,v)=
   let val n = Sym.hash(s)
       val l = IntMap.apply(map,n) handle IntMap.MapFail => []
       val newmap= IntMap.update(map,n,(s,v)::l)
    in TBL newmap
   end 

end;

functor FastIntMap(): IntMapSig=
struct  (* dummy implementation of int maps *)
  datatype 'a map = N of int * 'a * 'a map * 'a map  
                  | EMPTY
  val emptyMap = EMPTY
  exception MapFail
  fun apply _ = raise MapFail;
  fun update _ = raise MapFail;
end;

functor ValFct(): ValSig=
struct 
  type value = real
end;

functor SymFct(): SymSig=
struct
  type sym = string
  fun hash(s:sym)= ord s
end;

structure MyTbl=
SymTblFct(structure IntMap = FastIntMap()
          structure Val = ValFct()
          structure Sym = SymFct()
         );
              
open MyTbl;
  
update(emptyTable,"ape",10.0);
