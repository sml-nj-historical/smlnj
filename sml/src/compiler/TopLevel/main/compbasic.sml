(* COPYRIGHT (c) 1996 Bell Laboratories *)
(* compbasic.sml *)

structure CompBasic : COMPBASIC = 
struct

type source     = Source.inputSource
type ast        = Ast.dec    
type absyn      = Absyn.dec           
type flint      = FLINT.prog         
type dsegment   = Word8Vector.vector
type csegments  = CodeObj.csegments
type object     = Unsafe.Object.object            
type executable = CodeObj.executable

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
               mkMkStamp : unit -> Stamps.generator) : compInfo = 
  let val {error,errorMatch,anyErrors} = ErrorMsg.errors source
      val _ = LambdaVar.clear()
      val g = mkMkStamp ()
   in {mkStamp = fn () => Stamps.fresh g,
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

