(* ______________________________________________________________________
   smldec-to-html.sml
   ______________________________________________________________________ *)

structure Test = struct

structure H4U = HTML4Utils

(* ____________________________________________________________ *)
(* Most of the following set of functions were automatically generated.
 *)

local
    open Ast
in
    fun handleSigConst NoSig = H4U.Nd(Atom.atom "NoSig", nil)
      | handleSigConst (Opaque _) = H4U.Nd(Atom.atom "Opaque", nil)
      | handleSigConst (Transparent _) = H4U.Nd(Atom.atom "Transparent", nil)
    and handleExp (AndalsoExp _) = H4U.Nd(Atom.atom "AndalsoExp", nil)
      | handleExp (AppExp _) = H4U.Nd(Atom.atom "AppExp", nil)
      | handleExp (CaseExp _) = H4U.Nd(Atom.atom "CaseExp", nil)
      | handleExp (CharExp _) = H4U.Nd(Atom.atom "CharExp", nil)
      | handleExp (ConstraintExp _) = H4U.Nd(Atom.atom "ConstraintExp", nil)
      | handleExp (FlatAppExp _) = H4U.Nd(Atom.atom "FlatAppExp", nil)
      | handleExp (FnExp _) = H4U.Nd(Atom.atom "FnExp", nil)
      | handleExp (HandleExp _) = H4U.Nd(Atom.atom "HandleExp", nil)
      | handleExp (IfExp _) = H4U.Nd(Atom.atom "IfExp", nil)
      | handleExp (IntExp _) = H4U.Nd(Atom.atom "IntExp", nil)
      | handleExp (LetExp _) = H4U.Nd(Atom.atom "LetExp", nil)
      | handleExp (ListExp _) = H4U.Nd(Atom.atom "ListExp", nil)
      | handleExp (MarkExp _) = H4U.Nd(Atom.atom "MarkExp", nil)
      | handleExp (OrelseExp _) = H4U.Nd(Atom.atom "OrelseExp", nil)
      | handleExp (RaiseExp _) = H4U.Nd(Atom.atom "RaiseExp", nil)
      | handleExp (RealExp _) = H4U.Nd(Atom.atom "RealExp", nil)
      | handleExp (RecordExp _) = H4U.Nd(Atom.atom "RecordExp", nil)
      | handleExp (SelectorExp _) = H4U.Nd(Atom.atom "SelectorExp", nil)
      | handleExp (SeqExp _) = H4U.Nd(Atom.atom "SeqExp", nil)
      | handleExp (StringExp _) = H4U.Nd(Atom.atom "StringExp", nil)
      | handleExp (TupleExp _) = H4U.Nd(Atom.atom "TupleExp", nil)
      | handleExp (VarExp _) = H4U.Nd(Atom.atom "VarExp", nil)
      | handleExp (VectorExp _) = H4U.Nd(Atom.atom "VectorExp", nil)
      | handleExp (WhileExp _) = H4U.Nd(Atom.atom "WhileExp", nil)
      | handleExp (WordExp _) = H4U.Nd(Atom.atom "WordExp", nil)
    and handleRule (Rule _) = H4U.Nd(Atom.atom "Rule", nil)
    and handlePat (AppPat _) = H4U.Nd(Atom.atom "AppPat", nil)
      | handlePat (CharPat _) = H4U.Nd(Atom.atom "CharPat", nil)
      | handlePat (ConstraintPat _) = H4U.Nd(Atom.atom "ConstraintPat", nil)
      | handlePat (FlatAppPat _) = H4U.Nd(Atom.atom "FlatAppPat", nil)
      | handlePat (IntPat _) = H4U.Nd(Atom.atom "IntPat", nil)
      | handlePat (LayeredPat _) = H4U.Nd(Atom.atom "LayeredPat", nil)
      | handlePat (ListPat _) = H4U.Nd(Atom.atom "ListPat", nil)
      | handlePat (MarkPat _) = H4U.Nd(Atom.atom "MarkPat", nil)
      | handlePat (OrPat _) = H4U.Nd(Atom.atom "OrPat", nil)
      | handlePat (RecordPat _) = H4U.Nd(Atom.atom "RecordPat", nil)
      | handlePat (StringPat _) = H4U.Nd(Atom.atom "StringPat", nil)
      | handlePat (TuplePat _) = H4U.Nd(Atom.atom "TuplePat", nil)
      | handlePat (VarPat _) = H4U.Nd(Atom.atom "VarPat", nil)
      | handlePat (VectorPat _) = H4U.Nd(Atom.atom "VectorPat", nil)
      | handlePat WildPat = H4U.Nd(Atom.atom "WildPat", nil)
      | handlePat (WordPat _) = H4U.Nd(Atom.atom "WordPat", nil)
    and handleStrexp (AppStr _) = H4U.Nd(Atom.atom "AppStr", nil)
      | handleStrexp (AppStrI _) = H4U.Nd(Atom.atom "AppStrI", nil)
      | handleStrexp (BaseStr _) = H4U.Nd(Atom.atom "BaseStr", nil)
      | handleStrexp (ConstrainedStr _) = H4U.Nd(Atom.atom "ConstrainedStr",
                                                 nil)
      | handleStrexp (LetStr _) = H4U.Nd(Atom.atom "LetStr", nil)
      | handleStrexp (MarkStr _) = H4U.Nd(Atom.atom "MarkStr", nil)
      | handleStrexp (VarStr _) = H4U.Nd(Atom.atom "VarStr", nil)
    and handleFctexp (AppFct _) = H4U.Nd(Atom.atom "AppFct", nil)
      | handleFctexp (BaseFct _) = H4U.Nd(Atom.atom "BaseFct", nil)
      | handleFctexp (LetFct _) = H4U.Nd(Atom.atom "LetFct", nil)
      | handleFctexp (MarkFct _) = H4U.Nd(Atom.atom "MarkFct", nil)
      | handleFctexp (VarFct _) = H4U.Nd(Atom.atom "VarFct", nil)
    and handleWherespec (WhStruct _) = H4U.Nd(Atom.atom "WhStruct", nil)
      | handleWherespec (WhType _) = H4U.Nd(Atom.atom "WhType", nil)
    and handleSigexp (AugSig _) = H4U.Nd(Atom.atom "AugSig", nil)
      | handleSigexp (BaseSig _) = H4U.Nd(Atom.atom "BaseSig", nil)
      | handleSigexp (MarkSig _) = H4U.Nd(Atom.atom "MarkSig", nil)
      | handleSigexp (VarSig _) = H4U.Nd(Atom.atom "VarSig", nil)
    and handleFsigexp (BaseFsig _) = H4U.Nd(Atom.atom "BaseFsig", nil)
      | handleFsigexp (MarkFsig _) = H4U.Nd(Atom.atom "MarkFsig", nil)
      | handleFsigexp (VarFsig _) = H4U.Nd(Atom.atom "VarFsig", nil)
    and handleSpec (DataSpec _) = H4U.Nd(Atom.atom "DataSpec", nil)
      | handleSpec (ExceSpec _) = H4U.Nd(Atom.atom "ExceSpec", nil)
      | handleSpec (FctSpec _) = H4U.Nd(Atom.atom "FctSpec", nil)
      | handleSpec (IncludeSpec _) = H4U.Nd(Atom.atom "IncludeSpec", nil)
      | handleSpec (MarkSpec _) = H4U.Nd(Atom.atom "MarkSpec", nil)
      | handleSpec (ShareStrSpec _) = H4U.Nd(Atom.atom "ShareStrSpec", nil)
      | handleSpec (ShareTycSpec _) = H4U.Nd(Atom.atom "ShareTycSpec", nil)
      | handleSpec (StrSpec _) = H4U.Nd(Atom.atom "StrSpec", nil)
      | handleSpec (TycSpec _) = H4U.Nd(Atom.atom "TycSpec", nil)
      | handleSpec (ValSpec _) = H4U.Nd(Atom.atom "ValSpec", nil)
    and handleDec (AbsDec _) = H4U.Nd(Atom.atom "AbsDec", nil)
      | handleDec (AbstypeDec _) = H4U.Nd(Atom.atom "AbstypeDec", nil)
      | handleDec (DatatypeDec _) = H4U.Nd(Atom.atom "DatatypeDec", nil)
      | handleDec (ExceptionDec _) = H4U.Nd(Atom.atom "ExceptionDec", nil)
      | handleDec (FctDec _) = H4U.Nd(Atom.atom "FctDec", nil)
      | handleDec (FixDec _) = H4U.Nd(Atom.atom "FixDec", nil)
      | handleDec (FsigDec _) = H4U.Nd(Atom.atom "FsigDec", nil)
      | handleDec (FunDec _) = H4U.Nd(Atom.atom "FunDec", nil)
      | handleDec (LocalDec _) = H4U.Nd(Atom.atom "LocalDec", nil)
      | handleDec (MarkDec (thedec, _)) = handleDec thedec
      | handleDec (OpenDec _) = H4U.Nd(Atom.atom "OpenDec", nil)
      | handleDec (OvldDec _) = H4U.Nd(Atom.atom "OvldDec", nil)
      | handleDec (SeqDec decs) = H4U.Nd(Atom.atom "SeqDec",
                                         map handleDec decs)
      | handleDec (SigDec sigbs) = H4U.Nd(Atom.atom "SigDec",
                                          map handleSigb sigbs)
      | handleDec (StrDec strbs) = H4U.Nd(Atom.atom "StrDec",
                                          map handleStrb strbs)
      | handleDec (TypeDec _) = H4U.Nd(Atom.atom "TypeDec", nil)
      | handleDec (ValDec _) = H4U.Nd(Atom.atom "ValDec", nil)
      | handleDec (ValrecDec _) = H4U.Nd(Atom.atom "ValrecDec", nil)
    and handleVb (MarkVb _) = H4U.Nd(Atom.atom "MarkVb", nil)
      | handleVb (Vb _) = H4U.Nd(Atom.atom "Vb", nil)
    and handleRvb (MarkRvb _) = H4U.Nd(Atom.atom "MarkRvb", nil)
      | handleRvb (Rvb _) = H4U.Nd(Atom.atom "Rvb", nil)
    and handleFb (Fb _) = H4U.Nd(Atom.atom "Fb", nil)
      | handleFb (MarkFb _) = H4U.Nd(Atom.atom "MarkFb", nil)
    and handleClause (Clause _) = H4U.Nd(Atom.atom "Clause", nil)
    and handleTb (MarkTb _) = H4U.Nd(Atom.atom "MarkTb", nil)
      | handleTb (Tb _) = H4U.Nd(Atom.atom "Tb", nil)
    and handleDb (Db _) = H4U.Nd(Atom.atom "Db", nil)
      | handleDb (MarkDb _) = H4U.Nd(Atom.atom "MarkDb", nil)
    and handleDbrhs (Constrs _) = H4U.Nd(Atom.atom "Constrs", nil)
      | handleDbrhs (Repl _) = H4U.Nd(Atom.atom "Repl", nil)
    and handleEb (EbDef _) = H4U.Nd(Atom.atom "EbDef", nil)
      | handleEb (EbGen _) = H4U.Nd(Atom.atom "EbGen", nil)
      | handleEb (MarkEb _) = H4U.Nd(Atom.atom "MarkEb", nil)
    and handleStrb (MarkStrb _) = H4U.Nd(Atom.atom "MarkStrb", nil)
      | handleStrb (Strb _) = H4U.Nd(Atom.atom "Strb", nil)
    and handleFctb (Fctb _) = H4U.Nd(Atom.atom "Fctb", nil)
      | handleFctb (MarkFctb _) = H4U.Nd(Atom.atom "MarkFctb", nil)
    and handleSigb (MarkSigb _) = H4U.Nd(Atom.atom "MarkSigb", nil)
      | handleSigb (Sigb _) = H4U.Nd(Atom.atom "Sigb", nil)
    and handleFsigb (Fsigb _) = H4U.Nd(Atom.atom "Fsigb", nil)
      | handleFsigb (MarkFsigb _) = H4U.Nd(Atom.atom "MarkFsigb", nil)
    and handleTyvar (MarkTyv _) = H4U.Nd(Atom.atom "MarkTyv", nil)
      | handleTyvar (Tyv _) = H4U.Nd(Atom.atom "Tyv", nil)
    and handleTy (ConTy _) = H4U.Nd(Atom.atom "ConTy", nil)
      | handleTy (MarkTy _) = H4U.Nd(Atom.atom "MarkTy", nil)
      | handleTy (RecordTy _) = H4U.Nd(Atom.atom "RecordTy", nil)
      | handleTy (TupleTy _) = H4U.Nd(Atom.atom "TupleTy", nil)
      | handleTy (VarTy _) = H4U.Nd(Atom.atom "VarTy", nil)
end

(* ____________________________________________________________ *)

fun payloadToStr (_, []) = ""
  | payloadToStr (_, attrs as (_ :: _)) = " " ^ H4U.attrsToStr attrs

local
    open HTML4Tokens
in
    fun tokToString EOF = "EOF"
      | tokToString (OPENTAG (tagname, tagdata)) =
        String.concat ["OPENTAG ", Atom.toString tagname, " ",
                       payloadToStr tagdata]
      | tokToString (CLOSETAG tagname) = "CLOSETAG " ^ (Atom.toString tagname)
      | tokToString (DOCTYPE docdata) = docdata
      | tokToString (PCDATA pcdata) = pcdata
      | tokToString (COMMENT comment) = comment
      | tokToString (CHAR_REF refatom) = "CHAR REF " ^ (Atom.toString refatom)
      | tokToString (ENTITY_REF refatom) = "ENTITY REF " ^
                                           (Atom.toString refatom)
      | tokToString (XML_PROCESSING directive) = "XML DIRECTIVE " ^ directive
      | tokToString (STARTA payload) =
        "<A" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDA = "</A>"
      | tokToString (STARTABBR payload) =
        "<ABBR" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDABBR = "</ABBR>"
      | tokToString (STARTACRONYM payload) =
        "<ACRONYM" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDACRONYM = "</ACRONYM>"
      | tokToString (STARTADDRESS payload) =
        "<ADDRESS" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDADDRESS = "</ADDRESS>"
      | tokToString (STARTAREA payload) =
        "<AREA" ^ (payloadToStr payload) ^ ">"
      | tokToString (STARTB payload) =
        "<B" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDB = "</B>"
      | tokToString (STARTBASE payload) =
        "<BASE" ^ (payloadToStr payload) ^ ">"
      | tokToString (STARTBDO payload) =
        "<BDO" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDBDO = "</BDO>"
      | tokToString (STARTBIG payload) =
        "<BIG" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDBIG = "</BIG>"
      | tokToString (STARTBLOCKQUOTE payload) =
        "<BLOCKQUOTE" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDBLOCKQUOTE = "</BLOCKQUOTE>"
      | tokToString (STARTBODY payload) =
        "<BODY" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDBODY = "</BODY>"
      | tokToString (STARTBR payload) =
        "<BR" ^ (payloadToStr payload) ^ ">"
      | tokToString (STARTBUTTON payload) =
        "<BUTTON" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDBUTTON = "</BUTTON>"
      | tokToString (STARTCAPTION payload) =
        "<CAPTION" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDCAPTION = "</CAPTION>"
      | tokToString (STARTCITE payload) =
        "<CITE" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDCITE = "</CITE>"
      | tokToString (STARTCODE payload) =
        "<CODE" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDCODE = "</CODE>"
      | tokToString (STARTCOL payload) =
        "<COL" ^ (payloadToStr payload) ^ ">"
      | tokToString (STARTCOLGROUP payload) =
        "<COLGROUP" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDCOLGROUP = "</COLGROUP>"
      | tokToString (STARTDD payload) =
        "<DD" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDDD = "</DD>"
      | tokToString (STARTDEL payload) =
        "<DEL" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDDEL = "</DEL>"
      | tokToString (STARTDFN payload) =
        "<DFN" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDDFN = "</DFN>"
      | tokToString (STARTDIV payload) =
        "<DIV" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDDIV = "</DIV>"
      | tokToString (STARTDL payload) =
        "<DL" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDDL = "</DL>"
      | tokToString (STARTDT payload) =
        "<DT" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDDT = "</DT>"
      | tokToString (STARTEM payload) =
        "<EM" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDEM = "</EM>"
      | tokToString (STARTFIELDSET payload) =
        "<FIELDSET" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDFIELDSET = "</FIELDSET>"
      | tokToString (STARTFORM payload) =
        "<FORM" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDFORM = "</FORM>"
      | tokToString (STARTH1 payload) =
        "<H1" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDH1 = "</H1>"
      | tokToString (STARTH2 payload) =
        "<H2" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDH2 = "</H2>"
      | tokToString (STARTH3 payload) =
        "<H3" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDH3 = "</H3>"
      | tokToString (STARTH4 payload) =
        "<H4" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDH4 = "</H4>"
      | tokToString (STARTH5 payload) =
        "<H5" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDH5 = "</H5>"
      | tokToString (STARTH6 payload) =
        "<H6" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDH6 = "</H6>"
      | tokToString (STARTHEAD payload) =
        "<HEAD" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDHEAD = "</HEAD>"
      | tokToString (STARTHR payload) =
        "<HR" ^ (payloadToStr payload) ^ ">"
      | tokToString (STARTHTML payload) =
        "<HTML" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDHTML = "</HTML>"
      | tokToString (STARTI payload) =
        "<I" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDI = "</I>"
      | tokToString (STARTIMG payload) =
        "<IMG" ^ (payloadToStr payload) ^ ">"
      | tokToString (STARTINPUT payload) =
        "<INPUT" ^ (payloadToStr payload) ^ ">"
      | tokToString (STARTINS payload) =
        "<INS" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDINS = "</INS>"
      | tokToString (STARTKBD payload) =
        "<KBD" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDKBD = "</KBD>"
      | tokToString (STARTLABEL payload) =
        "<LABEL" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDLABEL = "</LABEL>"
      | tokToString (STARTLEGEND payload) =
        "<LEGEND" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDLEGEND = "</LEGEND>"
      | tokToString (STARTLI payload) =
        "<LI" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDLI = "</LI>"
      | tokToString (STARTLINK payload) =
        "<LINK" ^ (payloadToStr payload) ^ ">"
      | tokToString (STARTMAP payload) =
        "<MAP" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDMAP = "</MAP>"
      | tokToString (STARTMETA payload) =
        "<META" ^ (payloadToStr payload) ^ ">"
      | tokToString (STARTNOSCRIPT payload) =
        "<NOSCRIPT" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDNOSCRIPT = "</NOSCRIPT>"
      | tokToString (STARTOBJECT payload) =
        "<OBJECT" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDOBJECT = "</OBJECT>"
      | tokToString (STARTOL payload) =
        "<OL" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDOL = "</OL>"
      | tokToString (STARTOPTGROUP payload) =
        "<OPTGROUP" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDOPTGROUP = "</OPTGROUP>"
      | tokToString (STARTOPTION payload) =
        "<OPTION" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDOPTION = "</OPTION>"
      | tokToString (STARTP payload) =
        "<P" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDP = "</P>"
      | tokToString (STARTPARAM payload) =
        "<PARAM" ^ (payloadToStr payload) ^ ">"
      | tokToString (STARTPRE payload) =
        "<PRE" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDPRE = "</PRE>"
      | tokToString (STARTQ payload) =
        "<Q" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDQ = "</Q>"
      | tokToString (STARTSAMP payload) =
        "<SAMP" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDSAMP = "</SAMP>"
      | tokToString (STARTSCRIPT payload) =
        "<SCRIPT" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDSCRIPT = "</SCRIPT>"
      | tokToString (STARTSELECT payload) =
        "<SELECT" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDSELECT = "</SELECT>"
      | tokToString (STARTSMALL payload) =
        "<SMALL" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDSMALL = "</SMALL>"
      | tokToString (STARTSPAN payload) =
        "<SPAN" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDSPAN = "</SPAN>"
      | tokToString (STARTSTRONG payload) =
        "<STRONG" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDSTRONG = "</STRONG>"
      | tokToString (STARTSTYLE payload) =
        "<STYLE" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDSTYLE = "</STYLE>"
      | tokToString (STARTSUB payload) =
        "<SUB" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDSUB = "</SUB>"
      | tokToString (STARTSUP payload) =
        "<SUP" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDSUP = "</SUP>"
      | tokToString (STARTTABLE payload) =
        "<TABLE" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDTABLE = "</TABLE>"
      | tokToString (STARTTBODY payload) =
        "<TBODY" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDTBODY = "</TBODY>"
      | tokToString (STARTTD payload) =
        "<TD" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDTD = "</TD>"
      | tokToString (STARTTEXTAREA payload) =
        "<TEXTAREA" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDTEXTAREA = "</TEXTAREA>"
      | tokToString (STARTTFOOT payload) =
        "<TFOOT" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDTFOOT = "</TFOOT>"
      | tokToString (STARTTH payload) =
        "<TH" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDTH = "</TH>"
      | tokToString (STARTTHEAD payload) =
        "<THEAD" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDTHEAD = "</THEAD>"
      | tokToString (STARTTITLE payload) =
        "<TITLE" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDTITLE = "</TITLE>"
      | tokToString (STARTTR payload) =
        "<TR" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDTR = "</TR>"
      | tokToString (STARTTT payload) =
        "<TT" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDTT = "</TT>"
      | tokToString (STARTUL payload) =
        "<UL" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDUL = "</UL>"
      | tokToString (STARTVAR payload) =
        "<VAR" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDVAR = "</VAR>"
      | tokToString (STARTAPPLET payload) =
        "<APPLET" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDAPPLET = "</APPLET>"
      | tokToString (STARTBASEFONT payload) =
        "<BASEFONT" ^ (payloadToStr payload) ^ ">"
      | tokToString (STARTCENTER payload) =
        "<CENTER" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDCENTER = "</CENTER>"
      | tokToString (STARTDIR payload) =
        "<DIR" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDDIR = "</DIR>"
      | tokToString (STARTFONT payload) =
        "<FONT" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDFONT = "</FONT>"
      | tokToString (STARTIFRAME payload) =
        "<IFRAME" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDIFRAME = "</IFRAME>"
      | tokToString (STARTISINDEX payload) =
        "<ISINDEX" ^ (payloadToStr payload) ^ ">"
      | tokToString (STARTMENU payload) =
        "<MENU" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDMENU = "</MENU>"
      | tokToString (STARTS payload) =
        "<S" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDS = "</S>"
      | tokToString (STARTSTRIKE payload) =
        "<STRIKE" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDSTRIKE = "</STRIKE>"
      | tokToString (STARTU payload) =
        "<U" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDU = "</U>"
      | tokToString (STARTFRAME payload) =
        "<FRAME" ^ (payloadToStr payload) ^ ">"
      | tokToString (STARTFRAMESET payload) =
        "<FRAMESET" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDFRAMESET = "</FRAMESET>"
      | tokToString (STARTNOFRAMES payload) =
        "<NOFRAMES" ^ (payloadToStr payload) ^ ">"
      | tokToString ENDNOFRAMES = "</NOFRAMES>"
end

(* ____________________________________________________________ *)

fun tokIsSpace (HTML4Tokens.PCDATA pcstr) =
    let fun loop nil = true
          | loop (ch :: rst) = if Char.isSpace ch then loop rst else false
    in loop (String.explode pcstr) end
  | tokIsSpace _ = false

fun filterSpaceFromParseStream strm =
    let fun pred (H4U.VisitT tok) = not (tokIsSpace tok)
          | pred _ = true
    in H4U.stream_filter pred strm end

fun tokIsOpenTag tok = String.isPrefix "START" (HTML4Tokens.toString tok)

fun tokIsCloseTag tok = String.isPrefix "END" (HTML4Tokens.toString tok)

(* ____________________________________________________________ *)

val templateStream =
    let val instrm = TextIO.openIn "template.html"
        val template_pt_opt = HTML4.parseStream instrm
    in
        TextIO.closeIn instrm;
        case template_pt_opt of
            SOME (H4U.Nd(_, children)) =>
            H4U.stream_concatl (map H4U.parsetreeToVisitationStream children)
          | _ => H4U.StreamNil
    end handle ex => H4U.StreamNil

(* ____________________________________________________________ *)

fun outputHTMLParseStream (istrm, ostrm) =
    let fun visit (H4U.EnterNT _, indent) = indent ^ " "
          | visit (H4U.ExitNT _, indent) = String.extract(indent, 1, NONE)
          | visit (H4U.VisitT tok, indent) =
            (TextIO.output(ostrm, String.concat [indent, tokToString tok,
                                                 "\n"]);
             indent)
        val _ = H4U.stream_foldl visit "" istrm
    in () end

fun outputHTMLParseStream1 (istrm, ostrm) =
    let fun visit (H4U.EnterNT _, (indent,indents)) =
            (indent ^ " ", indent :: indents)
          | visit (H4U.ExitNT _, (_, indent :: indents)) = (indent, indents)
          | visit (H4U.VisitT tok, (indent, indents)) =
            (TextIO.output(ostrm, String.concat [indent, tokToString tok,
                                                 "\n"]);
             (if tokIsOpenTag tok
              then indent ^ " "
              else
                  if tokIsCloseTag tok
                  then String.extract(indent, 1, NONE)
                  else indent, indents))
        val _ = H4U.stream_foldl visit ("", []) istrm
    in () end

(* ____________________________________________________________ *)

exception NotPossible

structure CommentMap = ListMapFn(struct
                                 type ord_key = String.string
                                 val compare = String.compare
                                 end)

fun commentFilter commentMap =
    let fun guard (HTML4Tokens.COMMENT comStr) =
            CommentMap.inDomain(commentMap, comStr)
          | guard _ = false
        fun mapper (HTML4Tokens.COMMENT comStr) =
            CommentMap.lookup (commentMap, comStr)
          | mapper _ = raise NotPossible
    in H4U.parsetreeStreamMapTStream(guard, mapper) end

(* ____________________________________________________________ *)

fun parseFile filename =
    let val stream = TextIO.openIn filename
        val source = Source.newSource(filename, 1, stream, false,
                                      ErrorMsg.defaultConsumer())
        val result = SmlFile.parse source
    in Source.closeSource source; result end

(* ____________________________________________________________ *)

fun handleFile filename =
    let val intree = parseFile filename
        val strs = handleDec intree
        val commentMap =
            foldl CommentMap.insert' CommentMap.empty
                  [("<!--title-->",
                    H4U.stream_singleton (H4U.VisitT (HTML4Tokens.PCDATA
                                                          filename))),
                   ("<!--filename-->",
                    H4U.stream_singleton (H4U.VisitT (HTML4Tokens.PCDATA
                                                          filename))),
                   ("<!--pt-->", H4U.StreamNil)
                  ]
        val filterTemplate =
            (commentFilter commentMap) o filterSpaceFromParseStream
        val outstream = TextIO.openOut (filename ^ ".html")
        val _ = outputHTMLParseStream(filterTemplate templateStream, outstream)
    in TextIO.closeOut outstream end

(* ____________________________________________________________
   Main routine.
 *)

fun main (_, args) = (List.app handleFile args; OS.Process.success)
    handle ex => OS.Process.failure

end

(* ______________________________________________________________________
   End of smldec-to-html.sml
   ______________________________________________________________________ *)
