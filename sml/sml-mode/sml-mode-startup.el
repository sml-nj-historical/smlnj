;;;
;;; sample autoload entries for your site-lisp/site-start.el file
;;;

;;#ident "@(#)$Name$:$Id$"

;; don't forget to add the directory to your load-path
(add-to-list 'load-path "@elcdir@")

;; make sure the mode is loaded when necessary
(add-to-list 'auto-mode-alist '("\\.s\\(ml\\|ig\\)\\'" . sml-mode))

;; put this also if you feel like it (for SML/NJ's compilation manager)
(add-to-list 'completion-ignored-extensions "CM/")

;; the rest is the auto-generated autoloads

;;;### (autoloads (sml-mode) "sml-mode" "sml-mode.el" (14190 21314))
;;; Generated autoloads from sml-mode.el

(autoload (quote sml-mode) "sml-mode" "\
Major mode for editing ML code.
Entry to this mode runs the hooks on sml-mode-hook.
\\{sml-mode-map}" t nil)

;;;***

;;;### (autoloads (sml-load-file sml-send-buffer sml-send-region
;;;;;;  switch-to-sml run-sml) "sml-proc" "sml-proc.el" (14187 25166))
;;; Generated autoloads from sml-proc.el

(autoload (quote run-sml) "sml-proc" "\
Run an inferior ML process, input and output via buffer *sml*. 
With a prefix argument, this command allows you to specify any command
line options to pass to the complier. The command runs hook functions
on `comint-mode-hook' and `inferior-sml-mode-hook' in that order.

If there is a process already running in *sml*, just switch to that
buffer instead. 

In fact the name of the buffer created is chosen to reflect the name
of the program name specified by `sml-program-name', or entered at the
prompt. You can have several inferior ML process running, but only one
current one -- given by `sml-buffer' (qv).

\(Type \\[describe-mode] in the process buffer for a list of commands.)" t nil)

(autoload (quote switch-to-sml) "sml-proc" "\
Switch to the ML process buffer.
With prefix argument, positions cursor at point, otherwise at end of buffer." t nil)

(autoload (quote sml-send-region) "sml-proc" "\
Send current region to the inferior ML process.
Prefix argument means switch-to-sml afterwards.

The region is written out to a temporary file and a \"use <temp-file>\" command
is sent to the compiler.
See variables `sml-use-command'." t nil)

(autoload (quote sml-send-buffer) "sml-proc" "\
Send buffer to inferior shell running ML process. 
With a prefix argument switch to the sml buffer as well
\(cf. `sml-send-region')." t nil)

(autoload (quote sml-load-file) "sml-proc" "\
Load an ML file into the current inferior ML process. 
With a prefix argument switch to sml buffer as well.

This command uses the ML command template `sml-use-command' to construct
the command to send to the ML process; a trailing \";\\n\" will be added
automatically." t nil)

;;;***
