(* bug 587.sml *)
(* Compiler bug: ModuleUtil: Instantiate:getSigPos.2<Argument> *)

(* bug 587.sml *)
(* Compiler bug: ModuleUtil: Instantiate:getSigPos.2<Argument> *)

signature SEQUENCE =
  sig
  end;

signature SEMANTIC_VALUE = 
  sig
    type semantic_type  
    type semantic_value
    val add_semantic_value : semantic_value -> semantic_value -> unit
  end;

funsig MK_SEMANTIC_VALUE (Sequence : SEQUENCE) = SEMANTIC_VALUE;

functor MkWhole (functor MkSemanticValue : MK_SEMANTIC_VALUE) =
  struct
  end;
