structure Assert : ASSERT =
struct
    val enable = ref false
    val ignore = ref false

    type auxinfo = string

    exception Assert of auxinfo

    fun defaultHandler location =
        (TextIO.output (TextIO.stdErr,
                        location ^ ": Assertion failed!\n");
         (raise (Assert location)) : unit)

    val currentHandler = ref defaultHandler

    fun handler NONE = !currentHandler
      | handler (SOME h) = !currentHandler before currentHandler := h;

    fun fail aux = (!currentHandler) aux
end
