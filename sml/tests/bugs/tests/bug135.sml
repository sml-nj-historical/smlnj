(* bug135.sml *)

signature T =
sig
  type Shut
  type Open
  val Shut:Open->Shut
  and Open: Shut->Open
end;

signature U =
sig
  type Shut
  eqtype Open
  val Shut:Open->Shut
  and Open: Shut->Open
end;

(* Now we design a functor which simply wraps something up in order to shut it. *)

functor absT(type Open)  =
struct
  type Open = Open
  abstype Shut = SHUT of Open  with
    val Shut  = SHUT
    fun Open(SHUT x) = x
  end
end;

(* Now we instantiate it: *)

structure b:T = absT(type Open=int);

(* Compiler yields:

    structure b :
    sig
      eqtype Shut         <----- can't be right, surely
      eqtype Open
      val Open : Shut -> Open
      val Shut : Open -> Shut
    end

The equality on Shut has leaked, despite the fact that the actual
representation of Shut is an abstype. (The same happens if absT is
itself constrained to yield a T)
*)
b.Shut 3=b.Shut 4;
(*  val it = false : bool 

On the other hand using an abstraction binding
*)
structure ab :> T = absT(type Open=int);
(*
Compiler yields, correctly,

    structure ab :
      sig
	type Shut
	type Open
	val Open : Shut -> Open
	val Shut : Open -> Shut
      end

but I cannot actually apply ab.Shut to an integer (its domain is not
int, but an opaque and different type, namely ab.Open).  Now let's try
*)
structure au :> U = absT(type Open=int);
(*
Compiler yields, correctly,

    structure au :
    sig
      type Shut                                    
      eqtype Open
      val Open : Shut -> Open
      val Shut : Open -> Shut
    end

but I still can't apply au.Shut to an integer. Incidentally in my
original note I asked (a) whether I ought to be able to, (b) if so,
whether eqtype was not getting a bit overloaded [equality visible AND
representation visible] (c) if not, how could one do this sort of
thing at all?

Meanwhile
*)
structure argh:U = absT(type Open=int);
(*
still makes Open and Shut both eqtypes.  More bizarrely, we have
*)
abstype opaque = opaque of int with
 val hide = opaque
 val show = fn(opaque x)=>x
end;

structure biz:T = absT(type Open=opaque);
(*
Compiler yields

    structure biz :
      sig
	eqtype Shut                 <--- wow!
	type Open
	val Open : Shut -> Open
	val Shut : Open -> Shut
      end
*)
