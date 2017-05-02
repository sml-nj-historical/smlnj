(* Copyright 2003 by University of Chicago *)
(* src/Elaborator/print/ppast.sig *)
(* Jing Cao and Lukasz Ziarek *)

signature PPAST =

sig
	val ppExp	:StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> Ast.exp * int -> unit 
	val ppPat	:StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> Ast.pat * int -> unit
	val ppStrExp	:StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> Ast.strexp * int -> unit
	val ppFctExp	:StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> Ast.fctexp * int -> unit
	val ppWhereSpec :StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> Ast.wherespec * int -> unit
	val ppSigExp	:StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> Ast.sigexp * int -> unit
	val ppFsigExp	:StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> Ast.fsigexp * int -> unit
	val ppSpec	:StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> Ast.spec * int -> unit 
	val ppDec	:StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> Ast.dec * int -> unit
	val ppVb   	: StaticEnv.staticEnv * Source.inputSource option 
                         -> PrettyPrintNew.stream -> Ast.vb * int -> unit
  	val ppRvb  	: StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> Ast.rvb * int -> unit
	val ppFb	: StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> string -> Ast.fb * int -> unit
	val ppClause	: StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> Ast.clause * int -> unit
	val ppTb	: StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> Ast.tb * int -> unit
	val ppDb	: StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> Ast.db * int -> unit  
	val ppDbrhs	: StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> (Symbol.symbol * Ast.ty option) list * int -> unit
	val ppEb	: StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> Ast.eb * int -> unit
	val ppStrb	: StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> Ast.strb * int -> unit
	val ppFctb	: StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> Ast.fctb * int -> unit
	val ppTyvar	: StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> Ast.tyvar * int -> unit
	val ppTy	: StaticEnv.staticEnv * Source.inputSource option
               		 -> PrettyPrintNew.stream -> Ast.ty * int -> unit 
end
