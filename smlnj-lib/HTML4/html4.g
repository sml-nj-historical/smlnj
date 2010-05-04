(* ______________________________________________________________________
   html4.g
   ______________________________________________________________________ *)

%name HTML4;

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

document : cdata_opt (DOCTYPE cdata_opt)? (STARTHTML cdata_opt)? head
           (body | frameset)
           (ENDHTML cdata_opt)?
;

(* ______________________________________________________________________
   HEAD and related elements
   ______________________________________________________________________ *)

head : (STARTHEAD cdata_opt)?
       (head_content cdata_opt)*
       (ENDHEAD cdata_opt)?
;

head_content : title | base | script | style | meta | link | object

;

title : STARTTITLE cdata_opt ENDTITLE
;

base : STARTBASE
;

script : STARTSCRIPT cdata_opt ENDSCRIPT
;

style : STARTSTYLE cdata_opt ENDSTYLE
;

meta : STARTMETA
;

link : STARTLINK
;

object : STARTOBJECT (param | flow)* ENDOBJECT
;

param : STARTPARAM
;

(* ______________________________________________________________________
   BODY and related elements
   ______________________________________________________________________ *)

body : STARTBODY body_rest
     | (block | ins | del) body_rest
;

body_rest : (block | script | ins | del | cdata)* (ENDBODY cdata_opt)?
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

a : STARTA inline* ENDA ;

abbr : STARTABBR inline* ENDABBR ;

acronym : STARTACRONYM inline* ENDACRONYM ;

address : STARTADDRESS inline* ENDADDRESS ;

applet : STARTAPPLET (param | flow)* ENDAPPLET ;

area : STARTAREA ;

b : STARTB inline* ENDB ;

basefont : STARTBASEFONT ;

bdo : STARTBDO inline* ENDBDO ;

big : STARTBIG inline* ENDBIG ;

blockquote : STARTBLOCKQUOTE (block | script | cdata)+ ENDBLOCKQUOTE ;

br : STARTBR ;

button : STARTBUTTON flow* ENDBUTTON ;

caption : STARTCAPTION inline* ENDCAPTION ;

center : STARTCENTER flow* ENDCENTER ;

cite : STARTCITE inline* ENDCITE ;

code : STARTCODE inline* ENDCODE ;

col : STARTCOL ;

colgroup : STARTCOLGROUP cdata_opt (col cdata_opt)* ENDCOLGROUP? ;

dd : STARTDD flow* ENDDD? ;

del : STARTDEL flow* ENDDEL ;

dfn : STARTDFN inline* ENDDFN ;

dir : STARTDIR cdata_opt li+ ENDDIR ;

div : STARTDIV flow* ENDDIV ;

dl : STARTDL cdata_opt (dt | dd)+ ENDDL ;

dt : STARTDT inline* ENDDT? ;

em : STARTEM inline* ENDEM ;

fieldset : STARTFIELDSET cdata_opt legend flow* ENDFIELDSET ;

font : STARTFONT inline* ENDFONT ;

form : STARTFORM (cdata | block | script)+ ENDFORM ;

frame : STARTFRAME ;

frameset : STARTFRAMESET (frameset | frame | noframes | cdata)+ ENDFRAMESET ;

h1 : STARTH1 inline* ENDH1 ;

h2 : STARTH2 inline* ENDH2 ;

h3 : STARTH3 inline* ENDH3 ;

h4 : STARTH4 inline* ENDH4 ;

h5 : STARTH5 inline* ENDH5 ;

h6 : STARTH6 inline* ENDH6 ;

hr : STARTHR ;

i : STARTI inline* ENDI ;

iframe : STARTIFRAME flow* ENDIFRAME ;

img : STARTIMG ;

input : STARTINPUT ;

ins : STARTINS flow* ENDINS ;

isindex : STARTISINDEX ;

kbd : STARTKBD inline* ENDKBD ;

label : STARTLABEL inline* ENDLABEL ;

legend : STARTLEGEND inline* ENDLEGEND ;

li : STARTLI flow* ENDLI? ;

map : STARTMAP (cdata | block | area)+ ENDMAP ;

menu : STARTMENU cdata_opt li+ ENDMENU ;

noframes : STARTNOFRAMES body ENDNOFRAMES ;

noscript : STARTNOSCRIPT (cdata | block)+ ENDNOSCRIPT ;

ol : STARTOL cdata_opt li+ ENDOL ;

optgroup : STARTOPTGROUP cdata_opt option+ ENDOPTGROUP ;

option : STARTOPTION cdata_opt ENDOPTION? ;

p : STARTP inline* ENDP ;

pre : STARTPRE inline* ENDPRE ;

q : STARTQ inline* ENDQ ;

s : STARTS inline* ENDS ;

samp : STARTSAMP inline* ENDSAMP ;

select : STARTSELECT cdata_opt (optgroup cdata_opt | option)+ ENDSELECT ;

small : STARTSMALL inline* ENDSMALL ;

span : STARTSPAN inline* ENDSPAN ;

strike : STARTSTRIKE inline* ENDSTRIKE ;

strong : STARTSTRONG inline* ENDSTRONG ;

sub : STARTSUB inline* ENDSUB ;

sup : STARTSUP inline* ENDSUP ;

(* My reading of the HTML DTD indicates the following order of
elements is enforceable: *)

table : STARTTABLE cdata_opt (caption cdata_opt)? col_or_colgroups
        table_content ENDTABLE
;

(* The whole tr+ thing makes the original table production ambiguous:
   STARTTABLE ... thead? tfoot? tbody+ ENDTABLE *)

table_content 
        : thead tfoot? tbodies
        | tfoot tbodies
        | tbodies_nostart
;

col_or_colgroups : (* empty *)
                 | (col cdata_opt)+
                 | colgroup+
;

(*
tbody : (STARTTBODY cdata_opt)? tr+ ENDTBODY? ;
*)

tbodies_nostart : (STARTTBODY cdata_opt)? tr+ tbodies_rest?
;

tbodies : STARTTBODY cdata_opt tr+ tbodies_rest
;

tbodies_rest : ENDTBODY cdata_opt tbodies?
             | STARTBODY cdata_opt tr+ tbodies_rest?
;

td : STARTTD flow* (ENDTD cdata_opt)? ;

textarea : STARTTEXTAREA cdata_opt ENDTEXTAREA ;

tfoot : STARTTFOOT cdata_opt tr+ (ENDTFOOT cdata_opt)? ;

th : STARTTH flow* (ENDTH cdata_opt)? ;

thead : STARTTHEAD cdata_opt tr+ (ENDTHEAD cdata_opt)? ;

tr : STARTTR cdata_opt (th | td)+ (ENDTR cdata_opt)? ;

tt : STARTTT inline* ENDTT ;

u : STARTU inline* ENDU ;

ul : STARTUL cdata_opt li+ ENDUL ;

var : STARTVAR inline* ENDVAR ;

(* ______________________________________________________________________
   Miscellaneous data nonterminals
   ______________________________________________________________________ *)

cdata : (PCDATA | CHAR_REF | ENTITY_REF | COMMENT)
;

cdata_opt : cdata*
;

(* ______________________________________________________________________
   End of html4.g
   ______________________________________________________________________ *)
