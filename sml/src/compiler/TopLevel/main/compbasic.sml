(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* compbasic.sml *)

structure CompBasic : COMPBASIC = 
struct

type source     = Source.inputSource
type ast        = Ast.dec    
type absyn      = Absyn.dec           
type flint      = FLINT.prog         
type csegments  = {c0 : Word8Vector.vector, 
                   cn : Word8Vector.vector list, 
                   name : string option ref}
type object     = Unsafe.Object.object            
type executable = object Vector.vector -> object

type compInfo   = {mkStamp: unit -> Stamps.stamp,
                   mkLvar: Symbol.symbol option -> Access.lvar,
                   coreEnv: StaticEnv.staticEnv,
                   anyErrors: bool ref,
                   error: ErrorMsg.errorFn,
                   errorMatch: SourceMap.region -> string,
                   transform: absyn -> absyn,
                   sourceName : string}

fun mkCompInfo(source: source, coreEnv: StaticEnv.staticEnv,
	       transform : absyn -> absyn, 
               mkMkStamp : unit -> (unit -> Stamps.stamp)) : compInfo = 
  let val {error,errorMatch,anyErrors} = ErrorMsg.errors source
      val _ = LambdaVar.clear()
   in {mkStamp = mkMkStamp(),
       mkLvar = (fn NONE => LambdaVar.mkLvar ()
                  | SOME sym => LambdaVar.namedLvar sym),
       error = error,
       errorMatch = errorMatch,
       anyErrors = anyErrors,
       coreEnv = coreEnv,
       transform = transform,
       sourceName = #fileOpened source}
  end

fun anyErrors ({anyErrors=ref b,...} : compInfo) = b

end (* structure CompBasic *)

(*
 * $Log: compbasic.sml,v $
 * Revision 1.1.1.1  1998/04/08 18:39:15  george
 * Version 110.5
 *
 *)
