;;;
;;; sample autoload entries for your site-lisp/site-start.el file
;;;

;;#ident "@(#)$Name$:$Id$"

;; don't forget to add the directory to your load-path
(setq load-path (cons "@elcdir@" load-path))

;; make sure the mode is loaded when necessary
(setq auto-mode-alist
      (cons '("\\.s\\(ml\\|ig\\)\\'" . sml-mode) auto-mode-alist))
(autoload 'sml-mode "sml-mode" "Major mode for editing SML." t)
(autoload 'run-sml "sml-proc" "Run an inferior SML process" t)

;; put this also if you feel like it (for SML/NJ's compilation manager)
(setq completion-ignored-extensions (cons "CM/" completion-ignored-extensions))
