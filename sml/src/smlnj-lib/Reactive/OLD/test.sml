use "react.sml";

local
  open React
  infix &
  infix ||
  fun prAct s = React.action (fn _ => TextIO.print s)
in
val example = ((stop() & prAct "left ") || prAct "right ") & prAct "end\n"
end;
