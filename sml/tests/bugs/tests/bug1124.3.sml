(* bug1124.3.sml *)

signature SYNTAX =
sig
  type Name = string
  datatype Prim = p_plus
  datatype Atom = atom_int of int
		| atom_unit
  datatype Exp = exp_atom of Atom
	   | exp_var of Name
  and M = m_dynamic of Exp
end;

functor Eval (structure Syntax : SYNTAX) =
struct
  structure Syntax = Syntax
  open Syntax

  fun make_formal v = 
      let val bindings = ref []
	  fun bind (d:Exp) =
	      case (List.find (fn (l,r)=>l=d) (!bindings))
		of SOME(_,r) => (r:Exp)
          fun mf (m_dynamic d) = m_dynamic(bind d)
       in mf v
      end
end;
