(* bug 587.sml *)
(* Compiler bug: ModuleUtil: Instantiate:getSigPos.2<Argument> *)

signature SEQUENCE =
  sig
    exception LoopingError
    type 'a sequence
    val read        : '1a sequence  -> ('1a * '1a sequence) option
    val append : '1a list * '1a sequence -> '1a sequence
    val add_to : '1a sequence -> '1a sequence -> unit
    val app    : ('2a -> '2b) sequence -> '2a sequence 
                                            -> '2b sequence
    val value  : '1a -> '1a sequence
    val empty_sequence : unit -> '1a sequence
  end;

signature SEMANTIC_VALUE = 
  sig
    type 'a semantic_type
    type semantic_value
    type 'a sequence
    exception SemanticValueError of string
    val add_semantic_value : semantic_value -> semantic_value -> unit
    val cast_from : 'a semantic_type -> semantic_value -> 'a sequence
    val cast_to : 'a semantic_type -> 'a sequence -> semantic_value
    val void_semantic_value : semantic_value
  end;

funsig MK_SEMANTIC_VALUE (Sequence : SEQUENCE) = SEMANTIC_VALUE;

functor MkWhole (functor MkSemanticValue : MK_SEMANTIC_VALUE) =
  struct
  end;
