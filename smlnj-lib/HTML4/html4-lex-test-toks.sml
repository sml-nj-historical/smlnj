(* ______________________________________________________________________
   html4-lex-test-toks.sml
   ______________________________________________________________________ *)

structure HTML4Tokens = struct

datatype token = EOF
               | OPENTAG of Atom.atom * string
               | CLOSETAG of Atom.atom
               | COMMENT of string
               | PCDATA of string
               | DOCTYPE of string
               | CHAR_REF of Atom.atom
               | ENTITY_REF of Atom.atom
               | XML_PROCESSING of string
               (* HTML 4 element tokens. *)
               | STARTA of string
               | ENDA
               | STARTABBR of string
               | ENDABBR
               | STARTACRONYM of string
               | ENDACRONYM
               | STARTADDRESS of string
               | ENDADDRESS
               | STARTAPPLET of string
               | ENDAPPLET
               | STARTAREA of string
               (* No END tag for AREA element. *)
               | STARTB of string
               | ENDB
               | STARTBASE of string
               (* No END tag for BASE element. *)
               | STARTBASEFONT of string
               (* No END tag for BASEFONT element. *)
               | STARTBDO of string
               | ENDBDO
               | STARTBIG of string
               | ENDBIG
               | STARTBLOCKQUOTE of string
               | ENDBLOCKQUOTE
               | STARTBODY of string
               | ENDBODY
               | STARTBR of string
               (* No END tag for BR element. *)
               | STARTBUTTON of string
               | ENDBUTTON
               | STARTCAPTION of string
               | ENDCAPTION
               | STARTCENTER of string
               | ENDCENTER
               | STARTCITE of string
               | ENDCITE
               | STARTCODE of string
               | ENDCODE
               | STARTCOL of string
               (* No END tag for COL element. *)
               | STARTCOLGROUP of string
               | ENDCOLGROUP
               | STARTDD of string
               | ENDDD
               | STARTDEL of string
               | ENDDEL
               | STARTDFN of string
               | ENDDFN
               | STARTDIR of string
               | ENDDIR
               | STARTDIV of string
               | ENDDIV
               | STARTDL of string
               | ENDDL
               | STARTDT of string
               | ENDDT
               | STARTEM of string
               | ENDEM
               | STARTFIELDSET of string
               | ENDFIELDSET
               | STARTFONT of string
               | ENDFONT
               | STARTFORM of string
               | ENDFORM
               | STARTFRAME of string
               (* No END tag for FRAME element. *)
               | STARTFRAMESET of string
               | ENDFRAMESET
               | STARTH1 of string
               | ENDH1
               | STARTH2 of string
               | ENDH2
               | STARTH3 of string
               | ENDH3
               | STARTH4 of string
               | ENDH4
               | STARTH5 of string
               | ENDH5
               | STARTH6 of string
               | ENDH6
               | STARTHEAD of string
               | ENDHEAD
               | STARTHR of string
               (* No END tag for HR element. *)
               | STARTHTML of string
               | ENDHTML
               | STARTI of string
               | ENDI
               | STARTIFRAME of string
               | ENDIFRAME
               | STARTIMG of string
               (* No END tag for IMG element. *)
               | STARTINPUT of string
               (* No END tag for INPUT element. *)
               | STARTINS of string
               | ENDINS
               | STARTISINDEX of string
               (* No END tag for ISINDEX element. *)
               | STARTKBD of string
               | ENDKBD
               | STARTLABEL of string
               | ENDLABEL
               | STARTLEGEND of string
               | ENDLEGEND
               | STARTLI of string
               | ENDLI
               | STARTLINK of string
               (* No END tag for LINK element. *)
               | STARTMAP of string
               | ENDMAP
               | STARTMENU of string
               | ENDMENU
               | STARTMETA of string
               (* No END tag for META element. *)
               | STARTNOFRAMES of string
               | ENDNOFRAMES
               | STARTNOSCRIPT of string
               | ENDNOSCRIPT
               | STARTOBJECT of string
               | ENDOBJECT
               | STARTOL of string
               | ENDOL
               | STARTOPTGROUP of string
               | ENDOPTGROUP
               | STARTOPTION of string
               | ENDOPTION
               | STARTP of string
               | ENDP
               | STARTPARAM of string
               (* No END tag for PARAM element. *)
               | STARTPRE of string
               | ENDPRE
               | STARTQ of string
               | ENDQ
               | STARTS of string
               | ENDS
               | STARTSAMP of string
               | ENDSAMP
               | STARTSCRIPT of string
               | ENDSCRIPT
               | STARTSELECT of string
               | ENDSELECT
               | STARTSMALL of string
               | ENDSMALL
               | STARTSPAN of string
               | ENDSPAN
               | STARTSTRIKE of string
               | ENDSTRIKE
               | STARTSTRONG of string
               | ENDSTRONG
               | STARTSTYLE of string
               | ENDSTYLE
               | STARTSUB of string
               | ENDSUB
               | STARTSUP of string
               | ENDSUP
               | STARTTABLE of string
               | ENDTABLE
               | STARTTBODY of string
               | ENDTBODY
               | STARTTD of string
               | ENDTD
               | STARTTEXTAREA of string
               | ENDTEXTAREA
               | STARTTFOOT of string
               | ENDTFOOT
               | STARTTH of string
               | ENDTH
               | STARTTHEAD of string
               | ENDTHEAD
               | STARTTITLE of string
               | ENDTITLE
               | STARTTR of string
               | ENDTR
               | STARTTT of string
               | ENDTT
               | STARTU of string
               | ENDU
               | STARTUL of string
               | ENDUL
               | STARTVAR of string
               | ENDVAR

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
  | tokToString (STARTA payload) = "STARTA " ^ payload
  | tokToString ENDA = "ENDA"
  | tokToString (STARTABBR payload) = "STARTABBR " ^ payload
  | tokToString ENDABBR = "ENDABBR"
  | tokToString (STARTACRONYM payload) = "STARTACRONYM " ^ payload
  | tokToString ENDACRONYM = "ENDACRONYM"
  | tokToString (STARTADDRESS payload) = "STARTADDRESS " ^ payload
  | tokToString ENDADDRESS = "ENDADDRESS"
  | tokToString (STARTAREA payload) = "STARTAREA " ^ payload
  | tokToString (STARTB payload) = "STARTB " ^ payload
  | tokToString ENDB = "ENDB"
  | tokToString (STARTBASE payload) = "STARTBASE " ^ payload
  | tokToString (STARTBDO payload) = "STARTBDO " ^ payload
  | tokToString ENDBDO = "ENDBDO"
  | tokToString (STARTBIG payload) = "STARTBIG " ^ payload
  | tokToString ENDBIG = "ENDBIG"
  | tokToString (STARTBLOCKQUOTE payload) = "STARTBLOCKQUOTE " ^ payload
  | tokToString ENDBLOCKQUOTE = "ENDBLOCKQUOTE"
  | tokToString (STARTBODY payload) = "STARTBODY " ^ payload
  | tokToString ENDBODY = "ENDBODY"
  | tokToString (STARTBR payload) = "STARTBR " ^ payload
  | tokToString (STARTBUTTON payload) = "STARTBUTTON " ^ payload
  | tokToString ENDBUTTON = "ENDBUTTON"
  | tokToString (STARTCAPTION payload) = "STARTCAPTION " ^ payload
  | tokToString ENDCAPTION = "ENDCAPTION"
  | tokToString (STARTCITE payload) = "STARTCITE " ^ payload
  | tokToString ENDCITE = "ENDCITE"
  | tokToString (STARTCODE payload) = "STARTCODE " ^ payload
  | tokToString ENDCODE = "ENDCODE"
  | tokToString (STARTCOL payload) = "STARTCOL " ^ payload
  | tokToString (STARTCOLGROUP payload) = "STARTCOLGROUP " ^ payload
  | tokToString ENDCOLGROUP = "ENDCOLGROUP"
  | tokToString (STARTDD payload) = "STARTDD " ^ payload
  | tokToString ENDDD = "ENDDD"
  | tokToString (STARTDEL payload) = "STARTDEL " ^ payload
  | tokToString ENDDEL = "ENDDEL"
  | tokToString (STARTDFN payload) = "STARTDFN " ^ payload
  | tokToString ENDDFN = "ENDDFN"
  | tokToString (STARTDIV payload) = "STARTDIV " ^ payload
  | tokToString ENDDIV = "ENDDIV"
  | tokToString (STARTDL payload) = "STARTDL " ^ payload
  | tokToString ENDDL = "ENDDL"
  | tokToString (STARTDT payload) = "STARTDT " ^ payload
  | tokToString ENDDT = "ENDDT"
  | tokToString (STARTEM payload) = "STARTEM " ^ payload
  | tokToString ENDEM = "ENDEM"
  | tokToString (STARTFIELDSET payload) = "STARTFIELDSET " ^ payload
  | tokToString ENDFIELDSET = "ENDFIELDSET"
  | tokToString (STARTFORM payload) = "STARTFORM " ^ payload
  | tokToString ENDFORM = "ENDFORM"
  | tokToString (STARTH1 payload) = "STARTH1 " ^ payload
  | tokToString ENDH1 = "ENDH1"
  | tokToString (STARTH2 payload) = "STARTH2 " ^ payload
  | tokToString ENDH2 = "ENDH2"
  | tokToString (STARTH3 payload) = "STARTH3 " ^ payload
  | tokToString ENDH3 = "ENDH3"
  | tokToString (STARTH4 payload) = "STARTH4 " ^ payload
  | tokToString ENDH4 = "ENDH4"
  | tokToString (STARTH5 payload) = "STARTH5 " ^ payload
  | tokToString ENDH5 = "ENDH5"
  | tokToString (STARTH6 payload) = "STARTH6 " ^ payload
  | tokToString ENDH6 = "ENDH6"
  | tokToString (STARTHEAD payload) = "STARTHEAD " ^ payload
  | tokToString ENDHEAD = "ENDHEAD"
  | tokToString (STARTHR payload) = "STARTHR " ^ payload
  | tokToString (STARTHTML payload) = "STARTHTML " ^ payload
  | tokToString ENDHTML = "ENDHTML"
  | tokToString (STARTI payload) = "STARTI " ^ payload
  | tokToString ENDI = "ENDI"
  | tokToString (STARTIMG payload) = "STARTIMG " ^ payload
  | tokToString (STARTINPUT payload) = "STARTINPUT " ^ payload
  | tokToString (STARTINS payload) = "STARTINS " ^ payload
  | tokToString ENDINS = "ENDINS"
  | tokToString (STARTKBD payload) = "STARTKBD " ^ payload
  | tokToString ENDKBD = "ENDKBD"
  | tokToString (STARTLABEL payload) = "STARTLABEL " ^ payload
  | tokToString ENDLABEL = "ENDLABEL"
  | tokToString (STARTLEGEND payload) = "STARTLEGEND " ^ payload
  | tokToString ENDLEGEND = "ENDLEGEND"
  | tokToString (STARTLI payload) = "STARTLI " ^ payload
  | tokToString ENDLI = "ENDLI"
  | tokToString (STARTLINK payload) = "STARTLINK " ^ payload
  | tokToString (STARTMAP payload) = "STARTMAP " ^ payload
  | tokToString ENDMAP = "ENDMAP"
  | tokToString (STARTMETA payload) = "STARTMETA " ^ payload
  | tokToString (STARTNOSCRIPT payload) = "STARTNOSCRIPT " ^ payload
  | tokToString ENDNOSCRIPT = "ENDNOSCRIPT"
  | tokToString (STARTOBJECT payload) = "STARTOBJECT " ^ payload
  | tokToString ENDOBJECT = "ENDOBJECT"
  | tokToString (STARTOL payload) = "STARTOL " ^ payload
  | tokToString ENDOL = "ENDOL"
  | tokToString (STARTOPTGROUP payload) = "STARTOPTGROUP " ^ payload
  | tokToString ENDOPTGROUP = "ENDOPTGROUP"
  | tokToString (STARTOPTION payload) = "STARTOPTION " ^ payload
  | tokToString ENDOPTION = "ENDOPTION"
  | tokToString (STARTP payload) = "STARTP " ^ payload
  | tokToString ENDP = "ENDP"
  | tokToString (STARTPARAM payload) = "STARTPARAM " ^ payload
  | tokToString (STARTPRE payload) = "STARTPRE " ^ payload
  | tokToString ENDPRE = "ENDPRE"
  | tokToString (STARTQ payload) = "STARTQ " ^ payload
  | tokToString ENDQ = "ENDQ"
  | tokToString (STARTSAMP payload) = "STARTSAMP " ^ payload
  | tokToString ENDSAMP = "ENDSAMP"
  | tokToString (STARTSCRIPT payload) = "STARTSCRIPT " ^ payload
  | tokToString ENDSCRIPT = "ENDSCRIPT"
  | tokToString (STARTSELECT payload) = "STARTSELECT " ^ payload
  | tokToString ENDSELECT = "ENDSELECT"
  | tokToString (STARTSMALL payload) = "STARTSMALL " ^ payload
  | tokToString ENDSMALL = "ENDSMALL"
  | tokToString (STARTSPAN payload) = "STARTSPAN " ^ payload
  | tokToString ENDSPAN = "ENDSPAN"
  | tokToString (STARTSTRONG payload) = "STARTSTRONG " ^ payload
  | tokToString ENDSTRONG = "ENDSTRONG"
  | tokToString (STARTSTYLE payload) = "STARTSTYLE " ^ payload
  | tokToString ENDSTYLE = "ENDSTYLE"
  | tokToString (STARTSUB payload) = "STARTSUB " ^ payload
  | tokToString ENDSUB = "ENDSUB"
  | tokToString (STARTSUP payload) = "STARTSUP " ^ payload
  | tokToString ENDSUP = "ENDSUP"
  | tokToString (STARTTABLE payload) = "STARTTABLE " ^ payload
  | tokToString ENDTABLE = "ENDTABLE"
  | tokToString (STARTTBODY payload) = "STARTTBODY " ^ payload
  | tokToString ENDTBODY = "ENDTBODY"
  | tokToString (STARTTD payload) = "STARTTD " ^ payload
  | tokToString ENDTD = "ENDTD"
  | tokToString (STARTTEXTAREA payload) = "STARTTEXTAREA " ^ payload
  | tokToString ENDTEXTAREA = "ENDTEXTAREA"
  | tokToString (STARTTFOOT payload) = "STARTTFOOT " ^ payload
  | tokToString ENDTFOOT = "ENDTFOOT"
  | tokToString (STARTTH payload) = "STARTTH " ^ payload
  | tokToString ENDTH = "ENDTH"
  | tokToString (STARTTHEAD payload) = "STARTTHEAD " ^ payload
  | tokToString ENDTHEAD = "ENDTHEAD"
  | tokToString (STARTTITLE payload) = "STARTTITLE " ^ payload
  | tokToString ENDTITLE = "ENDTITLE"
  | tokToString (STARTTR payload) = "STARTTR " ^ payload
  | tokToString ENDTR = "ENDTR"
  | tokToString (STARTTT payload) = "STARTTT " ^ payload
  | tokToString ENDTT = "ENDTT"
  | tokToString (STARTUL payload) = "STARTUL " ^ payload
  | tokToString ENDUL = "ENDUL"
  | tokToString (STARTVAR payload) = "STARTVAR " ^ payload
  | tokToString ENDVAR = "ENDVAR"
  | tokToString (STARTAPPLET payload) = "STARTAPPLET " ^ payload
  | tokToString ENDAPPLET = "ENDAPPLET"
  | tokToString (STARTBASEFONT payload) = "STARTBASEFONT " ^ payload
  | tokToString (STARTCENTER payload) = "STARTCENTER " ^ payload
  | tokToString ENDCENTER = "ENDCENTER"
  | tokToString (STARTDIR payload) = "STARTDIR " ^ payload
  | tokToString ENDDIR = "ENDDIR"
  | tokToString (STARTFONT payload) = "STARTFONT " ^ payload
  | tokToString ENDFONT = "ENDFONT"
  | tokToString (STARTIFRAME payload) = "STARTIFRAME " ^ payload
  | tokToString ENDIFRAME = "ENDIFRAME"
  | tokToString (STARTISINDEX payload) = "STARTISINDEX " ^ payload
  | tokToString (STARTMENU payload) = "STARTMENU " ^ payload
  | tokToString ENDMENU = "ENDMENU"
  | tokToString (STARTS payload) = "STARTS " ^ payload
  | tokToString ENDS = "ENDS"
  | tokToString (STARTSTRIKE payload) = "STARTSTRIKE " ^ payload
  | tokToString ENDSTRIKE = "ENDSTRIKE"
  | tokToString (STARTU payload) = "STARTU " ^ payload
  | tokToString ENDU = "ENDU"
  | tokToString (STARTFRAME payload) = "STARTFRAME " ^ payload
  | tokToString (STARTFRAMESET payload) = "STARTFRAMESET " ^ payload
  | tokToString ENDFRAMESET = "ENDFRAMESET"
  | tokToString (STARTNOFRAMES payload) = "STARTNOFRAMES " ^ payload
  | tokToString ENDNOFRAMES = "ENDNOFRAMES"

  (* Should cause a "match redundant" error if code is all in synch: *)
  (* | tokToString _ = "???" *)

end

(* ______________________________________________________________________
   End of html4-lex-test-toks.sml
   ______________________________________________________________________ *)
