(* ______________________________________________________________________
   html4-token-utils.sml

   A set of utilities used for working with tokens used in the HTML 4
   parser.
   ______________________________________________________________________ *)

structure HTML4TokenUtils = struct

open HTML4Tokens

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

end

(* ______________________________________________________________________
   End of html4-token-utils.sml
   ______________________________________________________________________ *)
