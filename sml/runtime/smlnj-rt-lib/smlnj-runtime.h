
/*
 * This file was automatically generated by ml-idl
 * (Mon Aug  3 11:07:24 2009)
 */

#ifndef _SMLNJ_RUNTIME_H_
#define _SMLNJ_RUNTIME_H_

#include "ml-base.h"

#include "SMLNJ/ml-values.hxx"
#include "value-access.hxx"
typedef const char *idl_string;
typedef ML_Word8Vector ML_word8vec_t;
typedef ML_Option<ML_Word8Vector> ML_word8vec_opt_t;
typedef ML_Word8Array ML_word8arr_t;
typedef ML_String ML_charvec_t;
typedef ML_Option<ML_String> ML_charvec_opt_t;
typedef ml_val_t ML_chararr_t;
typedef ml_val_t ML_unit_t;
typedef ML_Bool ML_bool_t;
typedef ML_Int ML_int_t;
typedef ML_Int32 ML_int32_t;
typedef ML_String ML_string_t;
typedef ML_Option<ML_String> ML_string_opt_t;
typedef ML_List<ML_String> ML_string_list_t;
typedef ML_Option<ML_Int> ML_int_opt_t;
typedef ml_val_t ML_word_arr_opt_t;
typedef ml_val_t ML_int_arr_opt_t;
typedef ML_Option<ML_Tuple2<ML_Int32, ML_Int> > ML_int_pair_opt_t;
typedef ML_List<ML_Tuple2<ML_String, ML_Ref<ML_Int> > > ML_string_intref_list_t;
typedef ML_Tuple2<ML_Int, ML_String> ML_sysconst_t;
typedef ML_List<ML_Tuple2<ML_Int, ML_String> > ML_sysconst_list_t;
typedef ML_Option<ML_List<ML_Tuple2<ML_Int, ML_String> > > ML_sysconst_list_opt_t;
typedef ml_val_t ML_object_t;
typedef ML_Vector<ML_Value> ML_objectvec_t;
typedef ml_val_t ML_object_object_fn_t;
extern ML_word8arr_t allocCode (ml_state_t *msp,int nbytes);
extern ML_object_object_fn_t mkExec (ml_state_t *msp,ML_word8arr_t code,int entrypoint);
extern ML_objectvec_t mkLiterals (ml_state_t *msp,ML_word8vec_t arg);
extern void setTimer (ml_state_t *msp,ML_bool_t sts);
extern int getQuantum ();
extern void setTimeArray (ml_state_t *msp,ML_int_arr_opt_t prof_cnt_array);
extern void intervalTick (ml_state_t *msp,ML_int32_t *a,int *b);
extern void setIntTimer (ml_state_t *msp,ML_int_pair_opt_t itv);
extern ML_bool_t exportHeap (ml_state_t *msp,const char *fname);
#define IGNORE_SIG 0
#define DEFAULT_SIG 1
#define ENABLED_SIG 2
extern ML_sysconst_list_t listSignals (ml_state_t *msp);
extern int getSigState (ml_state_t *msp,ML_sysconst_t sc);
extern void setSigState (ml_state_t *msp,ML_sysconst_t sc,int i);
extern ML_sysconst_list_opt_t getSigMask (ml_state_t *msp);
extern void setSigMask (ml_state_t *msp,ML_sysconst_list_opt_t m);
extern void pauseUntilSig (ml_state_t *msp);
extern ML_string_list_t rawArgv (ml_state_t *msp);
extern void shiftArgv ();
extern ML_string_opt_t sysInfo (ml_state_t *msp,const char *name);
extern void gcControl (ml_state_t *msp,ML_string_intref_list_t cmds);
extern void debug (ml_state_t *msp,const char *s);
extern void dummy (const char *s);
extern ML_object_t recordConcat (ml_state_t *msp,ML_object_t rec1,ML_object_t rec2);
extern ML_object_t record1 (ml_state_t *msp,ML_object_t obj);
extern ML_object_t blastIn (ml_state_t *msp,const char *s);
extern ML_word8vec_t blastOut (ml_state_t *msp,ML_object_t obj);

#endif /* !_SMLNJ_RUNTIME_H_ */