structure AMD64_SMLNJMLTreeExt : SMLNJ_MLTREE_EXT =
struct
   type ('s,'r,'f,'c) sx = ('s,'r,'f,'c) AMD64InstrExt.sext
   type ('s,'r,'f,'c) rx = unit
   type ('s,'r,'f,'c) ccx = unit
   datatype ('s,'r,'f,'c) fx = 
       FSINE of 'f
     | FCOSINE of 'f
     | FTANGENT of 'f
end
