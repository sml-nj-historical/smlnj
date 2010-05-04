(* ______________________________________________________________________
   html4-lex-test-toks.sml
   ______________________________________________________________________ *)

structure HTML4Tokens = struct

type token_payload = string

fun payloadToString payload = payload

datatype token = EOF
               | OPENTAG of Atom.atom * token_payload
               | CLOSETAG of Atom.atom
               | COMMENT of token_payload
               | PCDATA of token_payload
               | DOCTYPE of token_payload
               | CHAR_REF of Atom.atom
               | ENTITY_REF of Atom.atom
               | XML_PROCESSING of token_payload
               (* HTML 4 element tokens. *)
               | STARTA of token_payload
               | ENDA
               | STARTABBR of token_payload
               | ENDABBR
               | STARTACRONYM of token_payload
               | ENDACRONYM
               | STARTADDRESS of token_payload
               | ENDADDRESS
               | STARTAPPLET of token_payload
               | ENDAPPLET
               | STARTAREA of token_payload
               (* No END tag for AREA element. *)
               | STARTB of token_payload
               | ENDB
               | STARTBASE of token_payload
               (* No END tag for BASE element. *)
               | STARTBASEFONT of token_payload
               (* No END tag for BASEFONT element. *)
               | STARTBDO of token_payload
               | ENDBDO
               | STARTBIG of token_payload
               | ENDBIG
               | STARTBLOCKQUOTE of token_payload
               | ENDBLOCKQUOTE
               | STARTBODY of token_payload
               | ENDBODY
               | STARTBR of token_payload
               (* No END tag for BR element. *)
               | STARTBUTTON of token_payload
               | ENDBUTTON
               | STARTCAPTION of token_payload
               | ENDCAPTION
               | STARTCENTER of token_payload
               | ENDCENTER
               | STARTCITE of token_payload
               | ENDCITE
               | STARTCODE of token_payload
               | ENDCODE
               | STARTCOL of token_payload
               (* No END tag for COL element. *)
               | STARTCOLGROUP of token_payload
               | ENDCOLGROUP
               | STARTDD of token_payload
               | ENDDD
               | STARTDEL of token_payload
               | ENDDEL
               | STARTDFN of token_payload
               | ENDDFN
               | STARTDIR of token_payload
               | ENDDIR
               | STARTDIV of token_payload
               | ENDDIV
               | STARTDL of token_payload
               | ENDDL
               | STARTDT of token_payload
               | ENDDT
               | STARTEM of token_payload
               | ENDEM
               | STARTFIELDSET of token_payload
               | ENDFIELDSET
               | STARTFONT of token_payload
               | ENDFONT
               | STARTFORM of token_payload
               | ENDFORM
               | STARTFRAME of token_payload
               (* No END tag for FRAME element. *)
               | STARTFRAMESET of token_payload
               | ENDFRAMESET
               | STARTH1 of token_payload
               | ENDH1
               | STARTH2 of token_payload
               | ENDH2
               | STARTH3 of token_payload
               | ENDH3
               | STARTH4 of token_payload
               | ENDH4
               | STARTH5 of token_payload
               | ENDH5
               | STARTH6 of token_payload
               | ENDH6
               | STARTHEAD of token_payload
               | ENDHEAD
               | STARTHR of token_payload
               (* No END tag for HR element. *)
               | STARTHTML of token_payload
               | ENDHTML
               | STARTI of token_payload
               | ENDI
               | STARTIFRAME of token_payload
               | ENDIFRAME
               | STARTIMG of token_payload
               (* No END tag for IMG element. *)
               | STARTINPUT of token_payload
               (* No END tag for INPUT element. *)
               | STARTINS of token_payload
               | ENDINS
               | STARTISINDEX of token_payload
               (* No END tag for ISINDEX element. *)
               | STARTKBD of token_payload
               | ENDKBD
               | STARTLABEL of token_payload
               | ENDLABEL
               | STARTLEGEND of token_payload
               | ENDLEGEND
               | STARTLI of token_payload
               | ENDLI
               | STARTLINK of token_payload
               (* No END tag for LINK element. *)
               | STARTMAP of token_payload
               | ENDMAP
               | STARTMENU of token_payload
               | ENDMENU
               | STARTMETA of token_payload
               (* No END tag for META element. *)
               | STARTNOFRAMES of token_payload
               | ENDNOFRAMES
               | STARTNOSCRIPT of token_payload
               | ENDNOSCRIPT
               | STARTOBJECT of token_payload
               | ENDOBJECT
               | STARTOL of token_payload
               | ENDOL
               | STARTOPTGROUP of token_payload
               | ENDOPTGROUP
               | STARTOPTION of token_payload
               | ENDOPTION
               | STARTP of token_payload
               | ENDP
               | STARTPARAM of token_payload
               (* No END tag for PARAM element. *)
               | STARTPRE of token_payload
               | ENDPRE
               | STARTQ of token_payload
               | ENDQ
               | STARTS of token_payload
               | ENDS
               | STARTSAMP of token_payload
               | ENDSAMP
               | STARTSCRIPT of token_payload
               | ENDSCRIPT
               | STARTSELECT of token_payload
               | ENDSELECT
               | STARTSMALL of token_payload
               | ENDSMALL
               | STARTSPAN of token_payload
               | ENDSPAN
               | STARTSTRIKE of token_payload
               | ENDSTRIKE
               | STARTSTRONG of token_payload
               | ENDSTRONG
               | STARTSTYLE of token_payload
               | ENDSTYLE
               | STARTSUB of token_payload
               | ENDSUB
               | STARTSUP of token_payload
               | ENDSUP
               | STARTTABLE of token_payload
               | ENDTABLE
               | STARTTBODY of token_payload
               | ENDTBODY
               | STARTTD of token_payload
               | ENDTD
               | STARTTEXTAREA of token_payload
               | ENDTEXTAREA
               | STARTTFOOT of token_payload
               | ENDTFOOT
               | STARTTH of token_payload
               | ENDTH
               | STARTTHEAD of token_payload
               | ENDTHEAD
               | STARTTITLE of token_payload
               | ENDTITLE
               | STARTTR of token_payload
               | ENDTR
               | STARTTT of token_payload
               | ENDTT
               | STARTU of token_payload
               | ENDU
               | STARTUL of token_payload
               | ENDUL
               | STARTVAR of token_payload
               | ENDVAR

val strict_tuple_list = [
    ("A", STARTA, SOME ENDA),
    ("ABBR", STARTABBR, SOME ENDABBR),
    ("ACRONYM", STARTACRONYM, SOME ENDACRONYM),
    ("ADDRESS", STARTADDRESS, SOME ENDADDRESS),
    ("AREA", STARTAREA, NONE),
    ("B", STARTB, SOME ENDB),
    ("BASE", STARTBASE, NONE),
    ("BDO", STARTBDO, SOME ENDBDO),
    ("BIG", STARTBIG, SOME ENDBIG),
    ("BLOCKQUOTE", STARTBLOCKQUOTE, SOME ENDBLOCKQUOTE),
    ("BODY", STARTBODY, SOME ENDBODY),
    ("BR", STARTBR, NONE),
    ("BUTTON", STARTBUTTON, SOME ENDBUTTON),
    ("CAPTION", STARTCAPTION, SOME ENDCAPTION),
    ("CITE", STARTCITE, SOME ENDCITE),
    ("CODE", STARTCODE, SOME ENDCODE),
    ("COL", STARTCOL, NONE),
    ("COLGROUP", STARTCOLGROUP, SOME ENDCOLGROUP),
    ("DD", STARTDD, SOME ENDDD),
    ("DEL", STARTDEL, SOME ENDDEL),
    ("DFN", STARTDFN, SOME ENDDFN),
    ("DIV", STARTDIV, SOME ENDDIV),
    ("DL", STARTDL, SOME ENDDL),
    ("DT", STARTDT, SOME ENDDT),
    ("EM", STARTEM, SOME ENDEM),
    ("FIELDSET", STARTFIELDSET, SOME ENDFIELDSET),
    ("FORM", STARTFORM, SOME ENDFORM),
    ("H1", STARTH1, SOME ENDH1),
    ("H2", STARTH2, SOME ENDH2),
    ("H3", STARTH3, SOME ENDH3),
    ("H4", STARTH4, SOME ENDH4),
    ("H5", STARTH5, SOME ENDH5),
    ("H6", STARTH6, SOME ENDH6),
    ("HEAD", STARTHEAD, SOME ENDHEAD),
    ("HR", STARTHR, NONE),
    ("HTML", STARTHTML, SOME ENDHTML),
    ("I", STARTI, SOME ENDI),
    ("IMG", STARTIMG, NONE),
    ("INPUT", STARTINPUT, NONE),
    ("INS", STARTINS, SOME ENDINS),
    ("KBD", STARTKBD, SOME ENDKBD),
    ("LABEL", STARTLABEL, SOME ENDLABEL),
    ("LEGEND", STARTLEGEND, SOME ENDLEGEND),
    ("LI", STARTLI, SOME ENDLI),
    ("LINK", STARTLINK, NONE),
    ("MAP", STARTMAP, SOME ENDMAP),
    ("META", STARTMETA, NONE),
    ("NOSCRIPT", STARTNOSCRIPT, SOME ENDNOSCRIPT),
    ("OBJECT", STARTOBJECT, SOME ENDOBJECT),
    ("OL", STARTOL, SOME ENDOL),
    ("OPTGROUP", STARTOPTGROUP, SOME ENDOPTGROUP),
    ("OPTION", STARTOPTION, SOME ENDOPTION),
    ("P", STARTP, SOME ENDP),
    ("PARAM", STARTPARAM, NONE),
    ("PRE", STARTPRE, SOME ENDPRE),
    ("Q", STARTQ, SOME ENDQ),
    ("SAMP", STARTSAMP, SOME ENDSAMP),
    ("SCRIPT", STARTSCRIPT, SOME ENDSCRIPT),
    ("SELECT", STARTSELECT, SOME ENDSELECT),
    ("SMALL", STARTSMALL, SOME ENDSMALL),
    ("SPAN", STARTSPAN, SOME ENDSPAN),
    ("STRONG", STARTSTRONG, SOME ENDSTRONG),
    ("STYLE", STARTSTYLE, SOME ENDSTYLE),
    ("SUB", STARTSUB, SOME ENDSUB),
    ("SUP", STARTSUP, SOME ENDSUP),
    ("TABLE", STARTTABLE, SOME ENDTABLE),
    ("TBODY", STARTTBODY, SOME ENDTBODY),
    ("TD", STARTTD, SOME ENDTD),
    ("TEXTAREA", STARTTEXTAREA, SOME ENDTEXTAREA),
    ("TFOOT", STARTTFOOT, SOME ENDTFOOT),
    ("TH", STARTTH, SOME ENDTH),
    ("THEAD", STARTTHEAD, SOME ENDTHEAD),
    ("TITLE", STARTTITLE, SOME ENDTITLE),
    ("TR", STARTTR, SOME ENDTR),
    ("TT", STARTTT, SOME ENDTT),
    ("UL", STARTUL, SOME ENDUL),
    ("VAR", STARTVAR, SOME ENDVAR)
]

val loose_tuple_list = [
    ("APPLET", STARTAPPLET, SOME ENDAPPLET),
    ("BASEFONT", STARTBASEFONT, NONE),
    ("CENTER", STARTCENTER, SOME ENDCENTER),
    ("DIR", STARTDIR, SOME ENDDIR),
    ("FONT", STARTFONT, SOME ENDFONT),
    ("IFRAME", STARTIFRAME, SOME ENDIFRAME),
    ("ISINDEX", STARTISINDEX, NONE),
    ("MENU", STARTMENU, SOME ENDMENU),
    ("S", STARTS, SOME ENDS),
    ("STRIKE", STARTSTRIKE, SOME ENDSTRIKE),
    ("U", STARTU, SOME ENDU)
]

val frameset_tuple_list = [
    ("FRAME", STARTFRAME, NONE),
    ("FRAMESET", STARTFRAMESET, SOME ENDFRAMESET),
    ("NOFRAMES", STARTNOFRAMES, SOME ENDNOFRAMES)
]

fun endTagNameTest ch = (case ch of
                             #" " => true | #"\t" => true | #"\r" => true 
                           | #"\n" => true | #">" => true | _ => false)

fun split ch_test = 
    let fun loop [] = []
          | loop (ch :: rst) = if ch_test ch then [] else ch :: (loop rst)
    in loop end

fun extractTag str =
    let val split_tag = split endTagNameTest
        val ch_list = case String.explode str of
                          #"<" :: #"/" :: rst => rst
                        | #"<" :: rst => rst
                        | rst => rst
        val ch_list' = split_tag ch_list
    in
        Atom.atom (String.implode (map Char.toUpper ch_list'))
    end

structure AtomMap : ORD_MAP = RedBlackMapFn(struct
                                            type ord_key = Atom.atom
                                            val compare = Atom.compare
                                            end)

fun element_tuple_to_ctor_maps ((tag_name, open_ctor, close_ctor_opt),
                                (open_map, close_map)) =
    let val tag_atom = Atom.atom tag_name
        val open_map' = AtomMap.insert(open_map, tag_atom, open_ctor)
        val close_map' = case close_ctor_opt of
                             NONE => close_map
                           | SOME close_tok => AtomMap.insert(
                                               close_map, tag_atom, close_tok)
    in (open_map', close_map') end

val (strict_open_map, strict_close_map) =
    foldl element_tuple_to_ctor_maps (AtomMap.empty, AtomMap.empty)
          strict_tuple_list

val (loose_open_map, loose_close_map) =
    foldl element_tuple_to_ctor_maps (strict_open_map, strict_close_map)
          loose_tuple_list

val (frameset_open_map, frameset_close_map) =
    foldl element_tuple_to_ctor_maps (strict_open_map, strict_close_map)
          frameset_tuple_list

val open_map_ref = ref strict_open_map

val close_map_ref = ref strict_close_map

fun mkOpenTag payload =
    let val tag_atom = extractTag payload
    in case AtomMap.find(!open_map_ref, tag_atom) of
           NONE => OPENTAG (tag_atom, payload)
         | SOME ctor => ctor payload
    end

fun mkCloseTag payload =
    let val tag_atom = extractTag payload
    in case AtomMap.find(!close_map_ref, tag_atom) of
           NONE => CLOSETAG tag_atom
         | SOME tok => tok
    end

fun tokToString EOF = "EOF"
  | tokToString (OPENTAG (tagname, tagdata)) =
    String.concat ["OPENTAG ", Atom.toString tagname, " ", tagdata]
  | tokToString (CLOSETAG tagname) = "CLOSETAG " ^ (Atom.toString tagname)
  | tokToString (DOCTYPE docdata) = "DOCTYPE " ^ docdata
  | tokToString (PCDATA pcdata) = ("PCDATA \"" ^ (String.toString pcdata)
                                   ^ "\"")
  | tokToString (COMMENT comment) = "COMMENT " ^ comment
  | tokToString (CHAR_REF refatom) = "CHAR REF " ^ (Atom.toString refatom)
  | tokToString (ENTITY_REF refatom) = "ENTITY REF " ^ (Atom.toString refatom)
  | tokToString (XML_PROCESSING directive) = "XML DIRECTIVE " ^ directive

  (* Automatically generated via helper.py: *)
  | tokToString (STARTA payload) = "STARTA " ^ (payloadToString payload)
  | tokToString ENDA = "ENDA"
  | tokToString (STARTABBR payload) = "STARTABBR " ^ (payloadToString payload)
  | tokToString ENDABBR = "ENDABBR"
  | tokToString (STARTACRONYM payload) = "STARTACRONYM " ^
                                         (payloadToString payload)
  | tokToString ENDACRONYM = "ENDACRONYM"
  | tokToString (STARTADDRESS payload) = "STARTADDRESS " ^
                                         (payloadToString payload)
  | tokToString ENDADDRESS = "ENDADDRESS"
  | tokToString (STARTAREA payload) = "STARTAREA " ^ (payloadToString payload)
  | tokToString (STARTB payload) = "STARTB " ^ (payloadToString payload)
  | tokToString ENDB = "ENDB"
  | tokToString (STARTBASE payload) = "STARTBASE " ^ (payloadToString payload)
  | tokToString (STARTBDO payload) = "STARTBDO " ^ (payloadToString payload)
  | tokToString ENDBDO = "ENDBDO"
  | tokToString (STARTBIG payload) = "STARTBIG " ^ (payloadToString payload)
  | tokToString ENDBIG = "ENDBIG"
  | tokToString (STARTBLOCKQUOTE payload) = "STARTBLOCKQUOTE " ^
                                            (payloadToString payload)
  | tokToString ENDBLOCKQUOTE = "ENDBLOCKQUOTE"
  | tokToString (STARTBODY payload) = "STARTBODY " ^ (payloadToString payload)
  | tokToString ENDBODY = "ENDBODY"
  | tokToString (STARTBR payload) = "STARTBR " ^ (payloadToString payload)
  | tokToString (STARTBUTTON payload) = "STARTBUTTON " ^
                                        (payloadToString payload)
  | tokToString ENDBUTTON = "ENDBUTTON"
  | tokToString (STARTCAPTION payload) = "STARTCAPTION " ^
                                         (payloadToString payload)
  | tokToString ENDCAPTION = "ENDCAPTION"
  | tokToString (STARTCITE payload) = "STARTCITE " ^ (payloadToString payload)
  | tokToString ENDCITE = "ENDCITE"
  | tokToString (STARTCODE payload) = "STARTCODE " ^ (payloadToString payload)
  | tokToString ENDCODE = "ENDCODE"
  | tokToString (STARTCOL payload) = "STARTCOL " ^ (payloadToString payload)
  | tokToString (STARTCOLGROUP payload) = "STARTCOLGROUP " ^
                                          (payloadToString payload)
  | tokToString ENDCOLGROUP = "ENDCOLGROUP"
  | tokToString (STARTDD payload) = "STARTDD " ^ (payloadToString payload)
  | tokToString ENDDD = "ENDDD"
  | tokToString (STARTDEL payload) = "STARTDEL " ^ (payloadToString payload)
  | tokToString ENDDEL = "ENDDEL"
  | tokToString (STARTDFN payload) = "STARTDFN " ^ (payloadToString payload)
  | tokToString ENDDFN = "ENDDFN"
  | tokToString (STARTDIV payload) = "STARTDIV " ^ (payloadToString payload)
  | tokToString ENDDIV = "ENDDIV"
  | tokToString (STARTDL payload) = "STARTDL " ^ (payloadToString payload)
  | tokToString ENDDL = "ENDDL"
  | tokToString (STARTDT payload) = "STARTDT " ^ (payloadToString payload)
  | tokToString ENDDT = "ENDDT"
  | tokToString (STARTEM payload) = "STARTEM " ^ (payloadToString payload)
  | tokToString ENDEM = "ENDEM"
  | tokToString (STARTFIELDSET payload) = "STARTFIELDSET " ^
                                          (payloadToString payload)
  | tokToString ENDFIELDSET = "ENDFIELDSET"
  | tokToString (STARTFORM payload) = "STARTFORM " ^ (payloadToString payload)
  | tokToString ENDFORM = "ENDFORM"
  | tokToString (STARTH1 payload) = "STARTH1 " ^ (payloadToString payload)
  | tokToString ENDH1 = "ENDH1"
  | tokToString (STARTH2 payload) = "STARTH2 " ^ (payloadToString payload)
  | tokToString ENDH2 = "ENDH2"
  | tokToString (STARTH3 payload) = "STARTH3 " ^ (payloadToString payload)
  | tokToString ENDH3 = "ENDH3"
  | tokToString (STARTH4 payload) = "STARTH4 " ^ (payloadToString payload)
  | tokToString ENDH4 = "ENDH4"
  | tokToString (STARTH5 payload) = "STARTH5 " ^ (payloadToString payload)
  | tokToString ENDH5 = "ENDH5"
  | tokToString (STARTH6 payload) = "STARTH6 " ^ (payloadToString payload)
  | tokToString ENDH6 = "ENDH6"
  | tokToString (STARTHEAD payload) = "STARTHEAD " ^ (payloadToString payload)
  | tokToString ENDHEAD = "ENDHEAD"
  | tokToString (STARTHR payload) = "STARTHR " ^ (payloadToString payload)
  | tokToString (STARTHTML payload) = "STARTHTML " ^ (payloadToString payload)
  | tokToString ENDHTML = "ENDHTML"
  | tokToString (STARTI payload) = "STARTI " ^ (payloadToString payload)
  | tokToString ENDI = "ENDI"
  | tokToString (STARTIMG payload) = "STARTIMG " ^ (payloadToString payload)
  | tokToString (STARTINPUT payload) = "STARTINPUT " ^
                                       (payloadToString payload)
  | tokToString (STARTINS payload) = "STARTINS " ^ (payloadToString payload)
  | tokToString ENDINS = "ENDINS"
  | tokToString (STARTKBD payload) = "STARTKBD " ^ (payloadToString payload)
  | tokToString ENDKBD = "ENDKBD"
  | tokToString (STARTLABEL payload) = "STARTLABEL " ^
                                       (payloadToString payload)
  | tokToString ENDLABEL = "ENDLABEL"
  | tokToString (STARTLEGEND payload) = "STARTLEGEND " ^
                                        (payloadToString payload)
  | tokToString ENDLEGEND = "ENDLEGEND"
  | tokToString (STARTLI payload) = "STARTLI " ^ (payloadToString payload)
  | tokToString ENDLI = "ENDLI"
  | tokToString (STARTLINK payload) = "STARTLINK " ^ (payloadToString payload)
  | tokToString (STARTMAP payload) = "STARTMAP " ^ (payloadToString payload)
  | tokToString ENDMAP = "ENDMAP"
  | tokToString (STARTMETA payload) = "STARTMETA " ^ (payloadToString payload)
  | tokToString (STARTNOSCRIPT payload) = "STARTNOSCRIPT " ^
                                          (payloadToString payload)
  | tokToString ENDNOSCRIPT = "ENDNOSCRIPT"
  | tokToString (STARTOBJECT payload) = "STARTOBJECT " ^
                                        (payloadToString payload)
  | tokToString ENDOBJECT = "ENDOBJECT"
  | tokToString (STARTOL payload) = "STARTOL " ^ (payloadToString payload)
  | tokToString ENDOL = "ENDOL"
  | tokToString (STARTOPTGROUP payload) = "STARTOPTGROUP " ^
                                          (payloadToString payload)
  | tokToString ENDOPTGROUP = "ENDOPTGROUP"
  | tokToString (STARTOPTION payload) = "STARTOPTION " ^
                                        (payloadToString payload)
  | tokToString ENDOPTION = "ENDOPTION"
  | tokToString (STARTP payload) = "STARTP " ^ (payloadToString payload)
  | tokToString ENDP = "ENDP"
  | tokToString (STARTPARAM payload) = "STARTPARAM " ^
                                       (payloadToString payload)
  | tokToString (STARTPRE payload) = "STARTPRE " ^ (payloadToString payload)
  | tokToString ENDPRE = "ENDPRE"
  | tokToString (STARTQ payload) = "STARTQ " ^ (payloadToString payload)
  | tokToString ENDQ = "ENDQ"
  | tokToString (STARTSAMP payload) = "STARTSAMP " ^ (payloadToString payload)
  | tokToString ENDSAMP = "ENDSAMP"
  | tokToString (STARTSCRIPT payload) = "STARTSCRIPT " ^
                                        (payloadToString payload)
  | tokToString ENDSCRIPT = "ENDSCRIPT"
  | tokToString (STARTSELECT payload) = "STARTSELECT " ^
                                        (payloadToString payload)
  | tokToString ENDSELECT = "ENDSELECT"
  | tokToString (STARTSMALL payload) = "STARTSMALL " ^
                                       (payloadToString payload)
  | tokToString ENDSMALL = "ENDSMALL"
  | tokToString (STARTSPAN payload) = "STARTSPAN " ^ (payloadToString payload)
  | tokToString ENDSPAN = "ENDSPAN"
  | tokToString (STARTSTRONG payload) = "STARTSTRONG " ^
                                        (payloadToString payload)
  | tokToString ENDSTRONG = "ENDSTRONG"
  | tokToString (STARTSTYLE payload) = "STARTSTYLE " ^
                                       (payloadToString payload)
  | tokToString ENDSTYLE = "ENDSTYLE"
  | tokToString (STARTSUB payload) = "STARTSUB " ^ (payloadToString payload)
  | tokToString ENDSUB = "ENDSUB"
  | tokToString (STARTSUP payload) = "STARTSUP " ^ (payloadToString payload)
  | tokToString ENDSUP = "ENDSUP"
  | tokToString (STARTTABLE payload) = "STARTTABLE " ^
                                       (payloadToString payload)
  | tokToString ENDTABLE = "ENDTABLE"
  | tokToString (STARTTBODY payload) = "STARTTBODY " ^
                                       (payloadToString payload)
  | tokToString ENDTBODY = "ENDTBODY"
  | tokToString (STARTTD payload) = "STARTTD " ^ (payloadToString payload)
  | tokToString ENDTD = "ENDTD"
  | tokToString (STARTTEXTAREA payload) = "STARTTEXTAREA " ^
                                          (payloadToString payload)
  | tokToString ENDTEXTAREA = "ENDTEXTAREA"
  | tokToString (STARTTFOOT payload) = "STARTTFOOT " ^
                                       (payloadToString payload)
  | tokToString ENDTFOOT = "ENDTFOOT"
  | tokToString (STARTTH payload) = "STARTTH " ^ (payloadToString payload)
  | tokToString ENDTH = "ENDTH"
  | tokToString (STARTTHEAD payload) = "STARTTHEAD " ^
                                       (payloadToString payload)
  | tokToString ENDTHEAD = "ENDTHEAD"
  | tokToString (STARTTITLE payload) = "STARTTITLE " ^
                                       (payloadToString payload)
  | tokToString ENDTITLE = "ENDTITLE"
  | tokToString (STARTTR payload) = "STARTTR " ^ (payloadToString payload)
  | tokToString ENDTR = "ENDTR"
  | tokToString (STARTTT payload) = "STARTTT " ^ (payloadToString payload)
  | tokToString ENDTT = "ENDTT"
  | tokToString (STARTUL payload) = "STARTUL " ^ (payloadToString payload)
  | tokToString ENDUL = "ENDUL"
  | tokToString (STARTVAR payload) = "STARTVAR " ^ (payloadToString payload)
  | tokToString ENDVAR = "ENDVAR"
  | tokToString (STARTAPPLET payload) = "STARTAPPLET " ^
                                        (payloadToString payload)
  | tokToString ENDAPPLET = "ENDAPPLET"
  | tokToString (STARTBASEFONT payload) = "STARTBASEFONT " ^
                                          (payloadToString payload)
  | tokToString (STARTCENTER payload) = "STARTCENTER " ^
                                        (payloadToString payload)
  | tokToString ENDCENTER = "ENDCENTER"
  | tokToString (STARTDIR payload) = "STARTDIR " ^ (payloadToString payload)
  | tokToString ENDDIR = "ENDDIR"
  | tokToString (STARTFONT payload) = "STARTFONT " ^ (payloadToString payload)
  | tokToString ENDFONT = "ENDFONT"
  | tokToString (STARTIFRAME payload) = "STARTIFRAME " ^
                                        (payloadToString payload)
  | tokToString ENDIFRAME = "ENDIFRAME"
  | tokToString (STARTISINDEX payload) = "STARTISINDEX " ^
                                         (payloadToString payload)
  | tokToString (STARTMENU payload) = "STARTMENU " ^ (payloadToString payload)
  | tokToString ENDMENU = "ENDMENU"
  | tokToString (STARTS payload) = "STARTS " ^ (payloadToString payload)
  | tokToString ENDS = "ENDS"
  | tokToString (STARTSTRIKE payload) = "STARTSTRIKE " ^
                                        (payloadToString payload)
  | tokToString ENDSTRIKE = "ENDSTRIKE"
  | tokToString (STARTU payload) = "STARTU " ^ (payloadToString payload)
  | tokToString ENDU = "ENDU"
  | tokToString (STARTFRAME payload) = "STARTFRAME " ^
                                       (payloadToString payload)
  | tokToString (STARTFRAMESET payload) = "STARTFRAMESET " ^
                                          (payloadToString payload)
  | tokToString ENDFRAMESET = "ENDFRAMESET"
  | tokToString (STARTNOFRAMES payload) = "STARTNOFRAMES " ^
                                          (payloadToString payload)
  | tokToString ENDNOFRAMES = "ENDNOFRAMES"

  (* Should cause a "match redundant" error if code is all in synch: *)
  (* | tokToString _ = "???" *)

end

(* ______________________________________________________________________
   End of html4-lex-test-toks.sml
   ______________________________________________________________________ *)
