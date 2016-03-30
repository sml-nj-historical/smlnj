
/*
 * This file was automatically generated by ml-idl
 * (Mon Aug  3 11:07:24 2009)
 */

#include "ml-base.h"
#include "ml-values.h"
#include "ml-objects.h"

#include "smlnj-runtime.h"

ml_val_t ml_stub_allocCode (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_nbytes;
  ML_word8arr_t ml_opresult;
  int ml_op_nbytes;
  ml_arg_nbytes = v;
  ml_op_nbytes = INT_MLtoC (ml_arg_nbytes);
  ml_opresult = allocCode (msp,ml_op_nbytes);
  ml_argresult = ml_opresult;
  return ml_argresult;
}

ml_val_t ml_stub_mkExec (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_code;
  ml_val_t ml_arg_entrypoint;
  ML_object_object_fn_t ml_opresult;
  ML_word8arr_t ml_op_code;
  int ml_op_entrypoint;
  ml_arg_code = REC_SEL (v,0);
  ml_arg_entrypoint = REC_SEL (v,1);
  ml_op_code = ml_arg_code;
  ml_op_entrypoint = INT_MLtoC (ml_arg_entrypoint);
  ml_opresult = mkExec (msp,ml_op_code, ml_op_entrypoint);
  ml_argresult = ml_opresult;
  return ml_argresult;
}

ml_val_t ml_stub_mkLiterals (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_arg;
  ML_objectvec_t ml_opresult;
  ML_word8vec_t ml_op_arg;
  ml_arg_arg = v;
  ml_op_arg = ml_arg_arg;
  ml_opresult = mkLiterals (msp,ml_op_arg);
  ml_argresult = ml_opresult;
  return ml_argresult;
}

ml_val_t ml_stub_setTimer (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_sts;
  ML_bool_t ml_op_sts;
  ml_arg_sts = v;
  ml_op_sts = ml_arg_sts;
  setTimer (msp,ml_op_sts);
  return ML_unit;
}

ml_val_t ml_stub_getQuantum (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  int ml_opresult;
  ml_opresult = getQuantum ();
  ml_argresult = INT_CtoML (ml_opresult);
  return ml_argresult;
}

ml_val_t ml_stub_setTimeArray (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_prof_cnt_array;
  ML_int_arr_opt_t ml_op_prof_cnt_array;
  ml_arg_prof_cnt_array = v;
  ml_op_prof_cnt_array = ml_arg_prof_cnt_array;
  setTimeArray (msp,ml_op_prof_cnt_array);
  return ML_unit;
}

ml_val_t ml_stub_intervalTick (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_a;
  ml_val_t ml_arg_b;
  ML_int32_t *ml_op_a;
  int *ml_op_b;
  ML_int32_t out_ml_op_a;
  int out_ml_op_b;
  ml_op_a = &out_ml_op_a;
  ml_op_b = &out_ml_op_b;
  intervalTick (msp,ml_op_a, ml_op_b);
  ml_arg_a = *(ml_op_a);
  ml_arg_b = INT_CtoML (*(ml_op_b));
  {
    ml_val_t    *__p = msp->ml_allocPtr;
    ml_val_t    r;
    *__p++ = MAKE_DESC(2,DTAG_record);
    *__p++ = ml_arg_a;
    *__p++ = ml_arg_b;
    r = PTR_CtoML(msp->ml_allocPtr + 1);
    msp->ml_allocPtr = __p;
    return r;  };
}

ml_val_t ml_stub_setIntTimer (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_itv;
  ML_int_pair_opt_t ml_op_itv;
  ml_arg_itv = v;
  ml_op_itv = ml_arg_itv;
  setIntTimer (msp,ml_op_itv);
  return ML_unit;
}

ml_val_t ml_stub_exportHeap (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_fname;
  ML_bool_t ml_opresult;
  char *ml_op_fname;
  ml_arg_fname = v;
  ml_op_fname = STR_MLtoC (ml_arg_fname);
  ml_opresult = exportHeap (msp,ml_op_fname);
  ml_argresult = ml_opresult;
  return ml_argresult;
}

ml_val_t ml_stub_listSignals (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ML_sysconst_list_t ml_opresult;
  ml_opresult = listSignals (msp);
  ml_argresult = ml_opresult;
  return ml_argresult;
}

ml_val_t ml_stub_getSigState (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_sc;
  int ml_opresult;
  ML_sysconst_t ml_op_sc;
  ml_arg_sc = v;
  ml_op_sc = ml_arg_sc;
  ml_opresult = getSigState (msp,ml_op_sc);
  ml_argresult = INT_CtoML (ml_opresult);
  return ml_argresult;
}

ml_val_t ml_stub_setSigState (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_sc;
  ml_val_t ml_arg_i;
  ML_sysconst_t ml_op_sc;
  int ml_op_i;
  ml_arg_sc = REC_SEL (v,0);
  ml_arg_i = REC_SEL (v,1);
  ml_op_sc = ml_arg_sc;
  ml_op_i = INT_MLtoC (ml_arg_i);
  setSigState (msp,ml_op_sc, ml_op_i);
  return ML_unit;
}

ml_val_t ml_stub_getSigMask (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ML_sysconst_list_opt_t ml_opresult;
  ml_opresult = getSigMask (msp);
  ml_argresult = ml_opresult;
  return ml_argresult;
}

ml_val_t ml_stub_setSigMask (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_m;
  ML_sysconst_list_opt_t ml_op_m;
  ml_arg_m = v;
  ml_op_m = ml_arg_m;
  setSigMask (msp,ml_op_m);
  return ML_unit;
}

ml_val_t ml_stub_pauseUntilSig (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  pauseUntilSig (msp);
  return ML_unit;
}

ml_val_t ml_stub_rawArgv (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ML_string_list_t ml_opresult;
  ml_opresult = rawArgv (msp);
  ml_argresult = ml_opresult;
  return ml_argresult;
}

ml_val_t ml_stub_shiftArgv (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  shiftArgv ();
  return ML_unit;
}

ml_val_t ml_stub_sysInfo (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_name;
  ML_string_opt_t ml_opresult;
  char *ml_op_name;
  ml_arg_name = v;
  ml_op_name = STR_MLtoC (ml_arg_name);
  ml_opresult = sysInfo (msp,ml_op_name);
  ml_argresult = ml_opresult;
  return ml_argresult;
}

ml_val_t ml_stub_gcControl (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_cmds;
  ML_string_intref_list_t ml_op_cmds;
  ml_arg_cmds = v;
  ml_op_cmds = ml_arg_cmds;
  gcControl (msp,ml_op_cmds);
  return ML_unit;
}

ml_val_t ml_stub_debug (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_s;
  char *ml_op_s;
  ml_arg_s = v;
  ml_op_s = STR_MLtoC (ml_arg_s);
  debug (msp,ml_op_s);
  return ML_unit;
}

ml_val_t ml_stub_dummy (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_s;
  char *ml_op_s;
  ml_arg_s = v;
  ml_op_s = STR_MLtoC (ml_arg_s);
  dummy (ml_op_s);
  return ML_unit;
}

ml_val_t ml_stub_recordConcat (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_rec1;
  ml_val_t ml_arg_rec2;
  ML_object_t ml_opresult;
  ML_object_t ml_op_rec1;
  ML_object_t ml_op_rec2;
  ml_arg_rec1 = REC_SEL (v,0);
  ml_arg_rec2 = REC_SEL (v,1);
  ml_op_rec1 = ml_arg_rec1;
  ml_op_rec2 = ml_arg_rec2;
  ml_opresult = recordConcat (msp,ml_op_rec1, ml_op_rec2);
  ml_argresult = ml_opresult;
  return ml_argresult;
}

ml_val_t ml_stub_record1 (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_obj;
  ML_object_t ml_opresult;
  ML_object_t ml_op_obj;
  ml_arg_obj = v;
  ml_op_obj = ml_arg_obj;
  ml_opresult = record1 (msp,ml_op_obj);
  ml_argresult = ml_opresult;
  return ml_argresult;
}

ml_val_t ml_stub_blastIn (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_s;
  ML_object_t ml_opresult;
  char *ml_op_s;
  ml_arg_s = v;
  ml_op_s = STR_MLtoC (ml_arg_s);
  ml_opresult = blastIn (msp,ml_op_s);
  ml_argresult = ml_opresult;
  return ml_argresult;
}

ml_val_t ml_stub_blastOut (ml_state_t *msp,ml_val_t v) {
  ml_val_t ml_result;
  ml_val_t ml_argresult;
  ml_val_t ml_arg_obj;
  ML_word8vec_t ml_opresult;
  ML_object_t ml_op_obj;
  ml_arg_obj = v;
  ml_op_obj = ml_arg_obj;
  ml_opresult = blastOut (msp,ml_op_obj);
  ml_argresult = ml_opresult;
  return ml_argresult;
}
