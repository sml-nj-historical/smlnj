(*
 * Extract information from user defined regions
 *)
signature REGION_PROPERTIES =
sig

   structure Region : REGION
   structure RegionInfo : REGION_INFO

   val readKind  : Region.region -> RegionInfo.kind
   val writeKind : Region.region -> RegionInfo.kind
   val readFrom  : Region.region -> int list (* uses *)
   val writeTo   : Region.region -> int list * int list (* defs/uses *)

end
