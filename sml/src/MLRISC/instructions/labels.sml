(* labels.sml
 *
 * COPYRIGHT (c) 1995 AT&T Bell Laboratories.
 *
 *)

signature LABEL =
  sig
    datatype label = Label of {id : int, addr : int ref, name:string}

    val newLabel : string -> label
    val nameOf   : label -> string
    val id       : label -> int
    val addrOf   : label -> int
    val reset	 : unit -> unit
    val setAddr  : label * int -> unit
  end


structure Label : LABEL =
  struct
    datatype label = Label of {id:int, addr:int ref, name:string}

    val cnt 		           = ref 0

    fun newLabel name = 
    let val id = !cnt
    in  cnt := id + 1; Label{id=id, addr=ref ~1, name=name} end
    fun nameOf(Label{id,name="",...})   = "LL" ^ Int.toString id
      | nameOf(Label{name, ...}) = name
    fun id(Label{id,...})          = id
    fun addrOf(Label{addr,...})    = !addr
    fun setAddr(Label{addr,...},n) = addr := n
    fun reset() 		   = cnt := 0
  end


