(* bug809.sml *)

funsig A() = sig end;

functor F(functor S : A) = struct end;

funsig FS () = sig end;
signature U = sig functor FF : FS end;

structure T : U =
  struct functor FF = F 
end;
