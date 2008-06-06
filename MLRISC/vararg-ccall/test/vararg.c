#include <stdio.h>

struct zipped_arg_s {
  union val_u {
    int i;
    double d;
    char* s;
  } val;
  long long kind;
  long long loc;
  long long ty;
};

extern void varargs (void* cFun, struct zipped_arg_s* args, struct zipped_arg_s* argsEnd);

void vararg_wrapper (void* cFun, struct zipped_arg_s* args, struct zipped_arg_s* argsEnd)
{
  printf ("vararg cFun=%p args=%p, end=%p\n", cFun, args, argsEnd);
  struct zipped_arg_s* tmp_args = args;

  for(tmp_args = args; tmp_args < argsEnd; tmp_args++)
    if ((int)tmp_args->ty == 32)
      printf ("arg=%d kind=%d loc=%d ty=%d\n", tmp_args->val.i, (int)tmp_args->kind, (int)tmp_args->loc, (int)tmp_args->ty);
    else 
      printf ("arg=%f kind=%d loc=%d ty=%d\n", tmp_args->val.d, (int)tmp_args->kind, (int)tmp_args->loc, (int)tmp_args->ty);

  varargs(cFun, args, argsEnd);
}

