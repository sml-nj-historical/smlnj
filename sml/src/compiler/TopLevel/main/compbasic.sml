(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* compbasic.sml *)

structure CompBasic : COMPBASIC = 
struct

type source     = Source.inputSource
type ast        = Ast.dec    
type absyn      = Absyn.dec           
type flint      = FLINT.prog         
type dsegment   = Word8Vector.vector
type csegments  = {c0 : Word8Vector.vector, 
                   cn : Word8Vector.vector list, 
                   data : Word8Vector.vector, 
                   name : string option ref}
type object     = Unsafe.Object.object            
type executable = object Vector.vector -> object

datatype importTree = ITNODE of (int * importTree) list

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
 * Revision 1.2  1998/05/20 18:40:32  george
 *   We now use a new cross-module linkage conventions; the import
 *   list of each module is now described as a tree which specifies
 *   in details about which component of a structure is imported.
 *   Also, each compilation unit now has a new data segment area,
 *   this also affects the changes on linking conventions and the
 *   binfile format. The new bin file format is described in
 *   batch/batchutil.sml.
 * 						-- zsh
 *
 * Revision 1.1.1.1  1998/04/08 18:39:15  george
 * Version 110.5
 *
 *)
