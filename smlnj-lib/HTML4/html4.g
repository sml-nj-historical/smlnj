(* ______________________________________________________________________
   html4.g
   ______________________________________________________________________ *)

%name HTML4;

%defs(

datatype 'a tree = Nd of 'a tree list
                 | Lf of 'a

fun optToList NONE = []
  | optToList (SOME thing) = [thing]

fun optListToList NONE = []
  | optListToList (SOME thing) = thing

);

%tokens        : OPENTAG of Atom.atom * string
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
;

%start document;

(*
%entry document, body, flow, block, inline;
*)

document : cdata_opt
           (DOCTYPE cdata_opt => ((Lf DOCTYPE) :: cdata_opt))?
           (STARTHTML cdata_opt => ((Lf STARTHTML) :: cdata_opt))?
           head
           (body | frameset)
           (ENDHTML cdata_opt => ((Lf ENDHTML) :: cdata_opt))?
           => (Nd (cdata_opt @ (optListToList SR1) @ (optListToList SR2) @
                   (head :: SR3 :: (optListToList SR4))))
;

(* ______________________________________________________________________
   HEAD and related elements
   ______________________________________________________________________ *)

head : (STARTHEAD cdata_opt => ((Lf STARTHEAD) :: cdata_opt))?
       (head_content cdata_opt => (head_content :: cdata_opt))*
       (ENDHEAD cdata_opt => ((Lf ENDHEAD) :: cdata_opt))?
       => (Nd ((optListToList SR1) @ (foldr op@ [] SR2) @ (optListToList SR3)))
;

head_content : title | base | script | style | meta | link | object
;

title : STARTTITLE cdata_opt ENDTITLE
        => (Nd ((Lf STARTTITLE) :: (cdata_opt @ [Lf ENDTITLE])))
;

base : STARTBASE
       => (Lf STARTBASE)
;

script : STARTSCRIPT cdata_opt ENDSCRIPT
         => (Nd ((Lf STARTSCRIPT) :: (cdata_opt @ [Lf ENDSCRIPT])))
;

style : STARTSTYLE cdata_opt ENDSTYLE
         => (Nd ((Lf STARTSTYLE) :: (cdata_opt @ [Lf ENDSTYLE])))
;

meta : STARTMETA
       => (Lf STARTMETA)
;

link : STARTLINK
       => (Lf STARTLINK)
;

object : STARTOBJECT (param | flow)* ENDOBJECT
         => (Nd ((Lf STARTOBJECT) :: (SR @ [Lf ENDOBJECT])))
;

param : STARTPARAM
       => (Lf STARTPARAM)
;

(* ______________________________________________________________________
   BODY and related elements
   ______________________________________________________________________ *)

body : STARTBODY body_rest
       => (Nd ((Lf STARTBODY) :: body_rest))
     | (block | ins | del) body_rest
       => (Nd (SR :: body_rest))
;

body_rest : (block | script | ins | del | cdata)*
            (ENDBODY cdata_opt => ((Lf ENDBODY) :: cdata_opt))?
            => (SR1 :: (optListToList SR2))
;

flow : block
     | inline
;

block : p 
      | heading
      | list
      | preformatted
      | dl
      | div
      | noscript
      | blockquote
      | form
      | hr
      | table
      | fieldset
      | address
      | block_loose
;

block_loose : center
            | isindex
;

heading : h1
        | h2
        | h3
        | h4
        | h5
        | h6
;

list : ul
     | ol
     | list_loose
;

list_loose : dir
           | menu
;

preformatted : pre
;

inline : fontstyle
       | phrase
       | special
       | formctrl
       | cdata
;

fontstyle : tt
          | i
          | b
          | big
          | small
          | fontstyle_loose
;

fontstyle_loose : u
                | s
                | strike
;

phrase : em
       | strong
       | dfn
       | code
       | samp
       | kbd
       | var
       | cite
       | abbr
       | acronym
;

special : a 
        | img
        | object
        | br
        | script
        | map
        | q
        | sub
        | sup
        | span
        | bdo
        | special_loose
;

special_loose : applet 
              | basefont
              | font
              | iframe
;

formctrl : input
         | select
         | textarea
         | label
         | button
;

(* Actual elements *)

a : STARTA inline* ENDA
         => (Nd ((Lf STARTA) :: (inline @ [Lf ENDA])))
;

abbr : STARTABBR inline* ENDABBR
         => (Nd ((Lf STARTABBR) :: (inline @ [Lf ENDABBR])))
;

acronym : STARTACRONYM inline* ENDACRONYM
         => (Nd ((Lf STARTACRONYM) :: (inline @ [Lf ENDACRONYM])))
;

address : STARTADDRESS inline* ENDADDRESS
         => (Nd ((Lf STARTADDRESS) :: (inline @ [Lf ENDADDRESS])))
;

applet : STARTAPPLET (param | flow)* ENDAPPLET
         => (Nd ((Lf STARTAPPLET) :: (SR @ [Lf ENDAPPLET])))
;

area : STARTAREA
       => (Lf STARTAREA)
;

b : STARTB inline* ENDB
         => (Nd ((Lf STARTB) :: (inline @ [Lf ENDB])))
;

basefont : STARTBASEFONT
       => (Lf STARTBASEFONT)
;

bdo : STARTBDO inline* ENDBDO
         => (Nd ((Lf STARTBDO) :: (inline @ [Lf ENDBDO])))
;

big : STARTBIG inline* ENDBIG
         => (Nd ((Lf STARTBIG) :: (inline @ [Lf ENDBIG])))
;

blockquote : STARTBLOCKQUOTE (block | script | cdata)+ ENDBLOCKQUOTE
         => (Nd ((Lf STARTBLOCKQUOTE) :: (SR @ [Lf ENDBLOCKQUOTE])))
;

br : STARTBR
       => (Lf STARTBR)
;

button : STARTBUTTON flow* ENDBUTTON
         => (Nd ((Lf STARTBUTTON) :: (flow @ [Lf ENDBUTTON])))
;

caption : STARTCAPTION inline* ENDCAPTION
          => (Nd ((Lf STARTCAPTION) :: (inline @ [Lf ENDCAPTION])))
;

center : STARTCENTER flow* ENDCENTER
         => (Nd ((Lf STARTCENTER) :: (flow @ [Lf ENDCENTER])))
;

cite : STARTCITE inline* ENDCITE
       => (Nd ((Lf STARTCITE) :: (inline @ [Lf ENDCITE])))
;

code : STARTCODE inline* ENDCODE
       => (Nd ((Lf STARTCODE) :: (inline @ [Lf ENDCODE])))
;

col : STARTCOL
      => (Lf STARTCOL)
;

colgroup : STARTCOLGROUP cdata_opt
           (col cdata_opt => (col :: cdata_opt))* ENDCOLGROUP?
           => (Nd ((Lf STARTCOLGROUP) :: (cdata_opt @ (foldr op@ [] SR) @
                                          (optToList ENDCOLGROUP))))
;

dd : STARTDD flow* ENDDD?
     => (Nd ((Lf STARTDD) :: (flow @ (optToList ENDDD))))
;

del : STARTDEL flow* ENDDEL
      => (Nd ((Lf STARTDEL) :: (flow @ [Lf ENDDEL])))
;

dfn : STARTDFN inline* ENDDFN
      => (Nd ((Lf STARTDFN) :: (inline @ [Lf ENDDFN])))
;

dir : STARTDIR cdata_opt li+ ENDDIR
      => (Nd ((Lf STARTDIR) :: (cdata_opt @ li @ [Lf ENDDIR])))
;

div : STARTDIV flow* ENDDIV
      => (Nd ((Lf STARTDIV) :: (flow @ [Lf ENDDIV])))
;

dl : STARTDL cdata_opt (dt | dd)+ ENDDL
     => (Nd ((Lf STARTDL) :: (cdata_opt @ SR @ [Lf ENDDL])))
;

dt : STARTDT inline* ENDDT?
     => (Nd ((Lf STARTDT) :: (inline @ (optToList ENDDT))))
;

em : STARTEM inline* ENDEM
     => (Nd ((Lf STARTEM) :: (inline @ [Lf ENDEM])))
;

fieldset : STARTFIELDSET cdata_opt legend flow* ENDFIELDSET
           => (Nd ((Lf STARTFIELDSET) :: (cdata_opt @ [legend] @ flow @
                                          [Lf ENDFIELDSET])))
;

font : STARTFONT inline* ENDFONT
       => (Nd ((Lf STARTFONT) :: (inline @ [Lf ENDFONT])))
;

form : STARTFORM (cdata | block | script)+ ENDFORM
       => (Nd ((Lf STARTFORM) :: (SR @ [Lf ENDFORM])))
;

frame : STARTFRAME
        => (Lf STARTFRAME)
;

frameset : STARTFRAMESET (frameset | frame | noframes | cdata)+ ENDFRAMESET
           => (Nd ((Lf STARTFRAMESET) :: (SR @ [Lf ENDFRAMESET])))
;

h1 : STARTH1 inline* ENDH1
     => (Nd ((Lf STARTH1) :: (inline @ [Lf ENDH1])))
;

h2 : STARTH2 inline* ENDH2
     => (Nd ((Lf STARTH2) :: (inline @ [Lf ENDH2])))
;

h3 : STARTH3 inline* ENDH3
     => (Nd ((Lf STARTH3) :: (inline @ [Lf ENDH3])))
;

h4 : STARTH4 inline* ENDH4
     => (Nd ((Lf STARTH4) :: (inline @ [Lf ENDH4])))
;

h5 : STARTH5 inline* ENDH5
     => (Nd ((Lf STARTH5) :: (inline @ [Lf ENDH5])))
;

h6 : STARTH6 inline* ENDH6
     => (Nd ((Lf STARTH6) :: (inline @ [Lf ENDH6])))
;

hr : STARTHR
     => (Lf STARTHR)
;

i : STARTI inline* ENDI
         => (Nd ((Lf STARTI) :: (inline @ [Lf ENDI])))
;

iframe : STARTIFRAME flow* ENDIFRAME
         => (Nd ((Lf STARTIFRAME) :: (flow @ [Lf ENDIFRAME])))
;

img : STARTIMG
       => (Lf STARTIMG)
;

input : STARTINPUT
       => (Lf STARTINPUT)
;

ins : STARTINS flow* ENDINS
         => (Nd ((Lf STARTINS) :: (flow @ [Lf ENDINS])))
;

isindex : STARTISINDEX
       => (Lf STARTISINDEX)
;

kbd : STARTKBD inline* ENDKBD
         => (Nd ((Lf STARTKBD) :: (inline @ [Lf ENDKBD])))
;

label : STARTLABEL inline* ENDLABEL
         => (Nd ((Lf STARTLABEL) :: (inline @ [Lf ENDLABEL])))
;

legend : STARTLEGEND inline* ENDLEGEND
         => (Nd ((Lf STARTLEGEND) :: (inline @ [Lf ENDLEGEND])))
;

li : STARTLI flow* ENDLI?
     => (Nd ((Lf STARTLI) :: (flow @ (optToList ENDLI))))
;

map : STARTMAP (cdata | block | area)+ ENDMAP
      => (Nd ((Lf STARTMAP) :: (SR @ [Lf ENDMAP])))
;

menu : STARTMENU cdata_opt li+ ENDMENU
       => (Nd ((Lf STARTMENU) :: (cdata_opt @ li @ [Lf ENDMENU])))
;

noframes : STARTNOFRAMES body ENDNOFRAMES
           => (Nd [Lf STARTNOFRAMES, body, Lf ENDNOFRAMES])
;

noscript : STARTNOSCRIPT (cdata | block)+ ENDNOSCRIPT
           => (Nd ((Lf STARTNOSCRIPT) :: (SR @ [Lf ENDNOSCRIPT])))
;

ol : STARTOL cdata_opt li+ ENDOL
     => (Nd ((Lf STARTOL) :: (cdata_opt @ li @ [Lf ENDOL])))
;

optgroup : STARTOPTGROUP cdata_opt option+ ENDOPTGROUP cdata_opt
           => (Nd ((Lf STARTOPTGROUP) :: (cdata_opt1 @ option @
                                          ((Lf ENDOPTGROUP) :: cdata_opt2))))
;

option : STARTOPTION cdata_opt
         (ENDOPTION cdata_opt => ((Lf ENDOPTION) :: cdata_opt))?
         => (Nd ((Lf STARTOPTION) :: (cdata_opt @ (optListToList SR))))
;

(* TODO: Making the ENDP optional, which is valid, causes
left-recursion for the inline* part.  This can be fixed by having a
two state flow nonterminal, which the older HTML library does. *)

p : STARTP inline* ENDP
    => (Nd ((Lf STARTP) :: (inline @ [Lf ENDP])))
;

pre : STARTPRE inline* ENDPRE
      => (Nd ((Lf STARTPRE) :: (inline @ [Lf ENDPRE])))
;

q : STARTQ inline* ENDQ
    => (Nd ((Lf STARTQ) :: (inline @ [Lf ENDQ])))
;

s : STARTS inline* ENDS
    => (Nd ((Lf STARTS) :: (inline @ [Lf ENDS])))
;

samp : STARTSAMP inline* ENDSAMP
       => (Nd ((Lf STARTSAMP) :: (inline @ [Lf ENDSAMP])))
;

select : STARTSELECT cdata_opt (optgroup | option)+ ENDSELECT
         => (Nd ((Lf STARTSELECT) :: (cdata_opt @ SR @ [Lf ENDSELECT])))
;

small : STARTSMALL inline* ENDSMALL
        => (Nd ((Lf STARTSMALL) :: (inline @ [Lf ENDSMALL])))
;

span : STARTSPAN inline* ENDSPAN
       => (Nd ((Lf STARTSPAN) :: (inline @ [Lf ENDSPAN])))
;

strike : STARTSTRIKE inline* ENDSTRIKE
         => (Nd ((Lf STARTSTRIKE) :: (inline @ [Lf ENDSTRIKE])))
;

strong : STARTSTRONG inline* ENDSTRONG
         => (Nd ((Lf STARTSTRONG) :: (inline @ [Lf ENDSTRONG])))
;

sub : STARTSUB inline* ENDSUB
      => (Nd ((Lf STARTSUB) :: (inline @ [Lf ENDSUB])))
;

sup : STARTSUP inline* ENDSUP
      => (Nd ((Lf STARTSUP) :: (inline @ [Lf ENDSUP])))
;

(* My reading of the HTML DTD indicates the following order of
elements is enforceable: *)

table : STARTTABLE cdata_opt
        (caption cdata_opt => (caption :: cdata_opt))?
        col_or_colgroups table_content ENDTABLE
        => (Nd ((Lf STARTTABLE) ::
                (cdata_opt @ (optListToList SR) @ col_or_colgroups @
                 table_content @ [Lf ENDTABLE])))
;

(* The whole tr+ thing makes the original table production ambiguous:
   STARTTABLE ... thead? tfoot? tbody+ ENDTABLE *)

table_content
        : thead tfoot? tbodies
          => (thead :: ((optToList tfoot)) @ tbodies)
        | tfoot tbodies
          => (tfoot :: tbodies)
        | tbodies_nostart
;

col_or_colgroups : (* empty *)
                   => ([])
                 | (col cdata_opt => (col :: cdata_opt))+
                   => (foldr op@ [] SR)
                 | colgroup+
;


tbodies_nostart : (STARTTBODY cdata_opt => ((Lf STARTTBODY) :: cdata_opt))?
                  tr+ tbodies_rest?
                  => (let val (tbody_rest, tbody_peers) =
                              case tbodies_rest of
                                  NONE => ([], [])
                                | SOME tbodies_tup => tbodies_tup
                      in (Nd ((optListToList SR) @ tr @ tbody_rest)) ::
                         tbody_peers end)
;

tbodies : STARTTBODY cdata_opt tr+ tbodies_rest
          => (let val (tbody_rest, tbody_peers) = tbodies_rest
              in (Nd ((Lf STARTTBODY) :: (cdata_opt @ tr @ tbody_rest))) ::
                 tbody_peers end)
;

tbodies_rest : ENDTBODY cdata_opt tbodies?
               => ((Lf ENDTBODY) :: cdata_opt, optListToList tbodies)
             | STARTTBODY cdata_opt tr+ tbodies_rest?
               => (let val (tbody_rest, tbody_peers) = tbodies_rest
                   in ([], (Nd ((Lf STARTTBODY) ::
                                (cdata_opt @ tr @ tbody_rest))) :: tbody_peers)
                   end)
;

td : STARTTD flow* (ENDTD cdata_opt => ((Lf ENDTD) :: cdata_opt))?
     => (Nd ((Lf STARTTD) :: (flow @ (optListToList SR))))
;

textarea : STARTTEXTAREA cdata_opt ENDTEXTAREA
           => (Nd ((Lf STARTTEXTAREA) :: (cdata_opt @ [Lf ENDTEXTAREA])))
;

tfoot : STARTTFOOT cdata_opt tr+
        (ENDTFOOT cdata_opt => ((Lf ENDTFOOT) :: cdata_opt))?
        => (Nd ((Lf STARTTFOOT) :: (cdata_opt @ tr @ (optListToList SR))))
;

th : STARTTH flow* (ENDTH cdata_opt => ((Lf ENDTH) :: cdata_opt))?
     => (Nd ((Lf STARTTH) :: (flow @ (optListToList SR))))
;

thead : STARTTHEAD cdata_opt tr+
        (ENDTHEAD cdata_opt => ((Lf ENDTHEAD) :: cdata_opt))?
        => (Nd ((Lf STARTTHEAD) :: (cdata_opt @ tr @ (optListToList SR))))
;

tr : STARTTR cdata_opt (th | td)+
     (ENDTR cdata_opt => ((Lf ENDTR) :: cdata_opt))?
     => (Nd ((Lf STARTTR) :: (cdata_opt @ SR1 @ (optListToList SR2))))
;

tt : STARTTT inline* ENDTT
     => (Nd ((Lf STARTTT) :: (inline @ [Lf ENDTT])))
;

u : STARTU inline* ENDU
    => (Nd ((Lf STARTU) :: (inline @ [Lf ENDU])))
;

ul : STARTUL cdata_opt li+ ENDUL
     => (Nd (((Lf STARTUL) :: (cdata_opt @ li @ [Lf ENDUL]))))
;

var : STARTVAR inline* ENDVAR
      => (Nd ((Lf STARTVAR) :: (inline @ [Lf ENDVAR])))
;

(* ______________________________________________________________________
   Miscellaneous data nonterminals
   ______________________________________________________________________ *)

cdata : (PCDATA | CHAR_REF | ENTITY_REF | COMMENT)
        => ((Lf SR) : HTML4Tokens.token tree)
;

cdata_opt : cdata* => (cdata : HTML4Tokens.token tree list)
;

(* ______________________________________________________________________
   End of html4.g
   ______________________________________________________________________ *)
