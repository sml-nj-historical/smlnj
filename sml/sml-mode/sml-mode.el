;;; sml-mode.el --- Major mode for editing (Standard) ML

;; Copyright (C) 1989       Lars Bo Nielsen
;; Copyright (C) 1994-1997  Matthew J. Morley
;; Copyright (C) 1999-2000  Stefan Monnier

;; $Revision$
;; $Date$

;; This file is not part of GNU Emacs, but it is distributed under the
;; same conditions.

;; ====================================================================

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.

;; ====================================================================


;;; Commentary:
;; 

;;; HISTORY

;; Still under construction: History obscure, needs a biographer as
;; well as a M-x doctor. Change Log on request.

;; Hacked by Olin Shivers for comint from Lars Bo Nielsen's sml.el.

;; Hacked by Matthew Morley to incorporate Fritz Knabe's hilite and
;; font-lock patterns, some of Steven Gilmore's (reduced) easy-menus,
;; and numerous bugs and bug-fixes.

;; Author: Lars Bo Nielsen
;;      Olin Shivers
;;	Fritz Knabe (?)
;;	Steven Gilmore (?)
;;	Matthew Morley <mjm@scs.leeds.ac.uk> (aka <matthew@verisity.com>)
;;	Matthias Blume <blume@cs.princeton.edu> (aka <blume@kurims.kyoto-u.ac.jp>)
;;      (Stefan Monnier) monnier@cs.yale.edu
;; Maintainer: (Stefan Monnier) monnier+lists/emacs/sml@tequila.cs.yale.edu
;; Keywords: SML

;;; DESCRIPTION

;; See accompanying info file: sml-mode.info

;;; FOR YOUR .EMACS FILE

;; If sml-mode.el lives in some non-standard directory, you must tell
;; emacs where to get it. This may or may not be necessary:

;; (setq load-path (cons (expand-file-name "~jones/lib/emacs") load-path))

;; Then to access the commands autoload sml-mode with that command:

;; (autoload 'sml-mode "sml-mode" "Major mode for editing ML programs." t)
;;
;; If files ending in ".sml" or ".ML" are hereafter considered to contain
;; Standard ML source, put their buffers into sml-mode automatically:

;; (setq auto-mode-alist
;;       (cons '(("\\.sml$" . sml-mode)
;;               ("\\.ML$"  . sml-mode)) auto-mode-alist))

;; Here's an example of setting things up in the sml-mode-hook:

;; (setq sml-mode-hook
;;       '(lambda() "ML mode hacks"
;;          (setq sml-indent-level 2         ; conserve on horiz. space
;;                indent-tabs-mode nil)))    ; whatever

;; sml-mode-hook is run whenever a new sml-mode buffer is created.

;; Finally, there are inferior-sml-{mode,load}-hooks -- see comments
;; in sml-proc.el. For much more information consult the mode's *info*
;; tree.

;;; Code:

(eval-when-compile (require 'cl))
(require 'sml-util)
(require 'sml-move)
(require 'sml-defs)

;;; VARIABLES CONTROLLING INDENTATION

(defcustom sml-indent-level 4
  "*Indentation of blocks in ML (see also `sml-structure-indent')."
  :group 'sml
  :type '(integer))

(defcustom sml-indent-args sml-indent-level
  "*Indentation of args placed on a separate line."
  :group 'sml
  :type '(integer))

;; (defvar sml-indent-align-args t
;;   "*Whether the arguments should be aligned.")

;; (defvar sml-case-indent nil
;;   "*How to indent case-of expressions.
;;     If t:   case expr                     If nil:   case expr of
;;               of exp1 => ...                            exp1 => ...
;;                | exp2 => ...                          | exp2 => ...

;; The first seems to be the standard in SML/NJ, but the second
;; seems nicer...")

(defcustom sml-electric-semi-mode nil
  "*If non-nil, `\;' will self insert, reindent the line, and do a newline.
If nil, just insert a `\;'.  (To insert while t, do: \\[quoted-insert] \;)."
  :group 'sml
  :type '(boolean))

;;; OTHER GENERIC MODE VARIABLES

(defvar sml-mode-info "sml-mode"
  "*Where to find Info file for `sml-mode'.
The default assumes the info file \"sml-mode.info\" is on Emacs' info
directory path.  If it is not, either put the file on the standard path
or set the variable `sml-mode-info' to the exact location of this file

  (setq sml-mode-info \"/usr/me/lib/info/sml-mode\")

in your .emacs file. You can always set it interactively with the
set-variable command.")

(defvar sml-mode-hook nil
  "*Run upon entering `sml-mode'.
This is a good place to put your preferred key bindings.")

(defvar sml-mode-abbrev-table nil "*Abbrev table for `sml-mode'.")

;;; CODE FOR SML-MODE

(defun sml-mode-info ()
  "Command to access the TeXinfo documentation for `sml-mode'.
See doc for the variable `sml-mode-info'."
  (interactive)
  (require 'info)
  (condition-case nil
      (info sml-mode-info)
    (error (progn
             (describe-variable 'sml-mode-info)
             (message "Can't find it... set this variable first!")))))


;;; Autoload functions -- no-doc is another idea cribbed from AucTeX!

(let ((sml-no-doc
       "This function is part of sml-proc, and has not yet been loaded.
Full documentation will be available after autoloading the function."))

  (autoload 'run-sml		"sml-proc"   sml-no-doc t)
  (autoload 'sml-compile	"sml-proc"   sml-no-doc t)
  (autoload 'sml-load-file	"sml-proc"   sml-no-doc t)
  (autoload 'switch-to-sml	"sml-proc"   sml-no-doc t)
  (autoload 'sml-send-region	"sml-proc"   sml-no-doc t)
  (autoload 'sml-send-buffer	"sml-proc"   sml-no-doc t))

;; font-lock setup

(defconst sml-keywords-regexp
  (sml-syms-re "abstraction" "abstype" "and" "andalso" "as" "before" "case"
	       "datatype" "else" "end" "eqtype" "exception" "do" "fn"
	       "fun" "functor" "handle" "if" "in" "include" "infix"
	       "infixr" "let" "local" "nonfix" "of" "op" "open" "orelse"
	       "overload" "raise" "rec" "sharing" "sig" "signature"
	       "struct" "structure" "then" "type" "val" "where" "while"
	       "with" "withtype" "o")
  "A regexp that matches any and all keywords of SML.")

(defconst sml-font-lock-keywords
  `(;;(sml-font-comments-and-strings)
    ("\\<\\(fun\\|and\\)\\s-+\\('\\sw+\\s-+\\)*\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (3 font-lock-function-name-face))
    ("\\<\\(\\(data\\|abs\\|with\\|eq\\)?type\\)\\s-+\\('\\sw+\\s-+\\)*\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (4 font-lock-type-def-face))
    ("\\<\\(val\\)\\s-+\\(\\sw+\\>\\s-*\\)?\\(\\sw+\\)\\s-*[=:]"
     (1 font-lock-keyword-face)
     ;;(6 font-lock-variable-def-face nil t)
     (3 font-lock-variable-name-face))
    ("\\<\\(structure\\|functor\\|abstraction\\)\\s-+\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-module-def-face))
    ("\\<\\(signature\\)\\s-+\\(\\sw+\\)"
     (1 font-lock-keyword-face)
     (2 font-lock-interface-def-face))
    
    (,sml-keywords-regexp . font-lock-keyword-face))
  "Regexps matching standard SML keywords.")

(defface font-lock-type-def-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight type definitions."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-type-def-face 'font-lock-type-def-face
  "Face name to use for type definitions.")

(defface font-lock-module-def-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight module definitions."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-module-def-face 'font-lock-module-def-face
  "Face name to use for module definitions.")

(defface font-lock-interface-def-face
  '((t (:bold t)))
  "Font Lock mode face used to highlight interface definitions."
  :group 'font-lock-highlighting-faces)
(defvar font-lock-interface-def-face 'font-lock-interface-def-face
  "Face name to use for interface definitions.")

;;; 
;;; Code to handle nested comments and unusual string escape sequences
;;; 

(defsyntax sml-syntax-prop-table
  '((?\\ . ".") (?* . "."))
  "Syntax table for text-properties")

;; For Emacsen that have no built-in support for nested comments
(defun sml-get-depth-st ()
  (save-excursion
    (let* ((disp (if (eq (char-before) ?\)) (progn (backward-char) -1) nil))
	   (foo (backward-char))
	   (disp (if (eq (char-before) ?\() (progn (backward-char) 0) disp))
	   (pt (point)))
      (when disp
	(let* ((depth
		(save-match-data
		  (if (re-search-backward "\\*)\\|(\\*" nil t)
		      (+ (or (get-char-property (point) 'comment-depth) 0)
			 (case (char-after) (?\( 1) (?* 0))
			 disp)
		    0)))
	       (depth (if (> depth 0) depth)))
	  (put-text-property pt (1+ pt) 'comment-depth depth)
	  (when depth sml-syntax-prop-table))))))

(defconst sml-font-lock-syntactic-keywords
  `(("^\\s-*\\(\\\\\\)" (1 ',sml-syntax-prop-table))
    ,@(unless sml-builtin-nested-comments-flag
	'(("(?\\(\\*\\))?" (1 (sml-get-depth-st)))))))

(defconst sml-font-lock-defaults
  '(sml-font-lock-keywords nil nil ((?_ . "w") (?' . "w")) nil
			   (font-lock-syntactic-keywords . sml-font-lock-syntactic-keywords)))


;;; MORE CODE FOR SML-MODE

;;;###Autoload
(add-to-list 'auto-mode-alist '("\\.s\\(ml\\|ig\\)\\'" . sml-mode))

;;;###Autoload
(define-derived-mode sml-mode fundamental-mode "SML"
  "\\<sml-mode-map>Major mode for editing ML code.
This mode runs `sml-mode-hook' just before exiting.
\\{sml-mode-map}"
  (set (make-local-variable 'font-lock-defaults) sml-font-lock-defaults)
  (set (make-local-variable 'outline-regexp) sml-outline-regexp)
  (sml-mode-variables))

(defun sml-mode-variables ()
  (set-syntax-table sml-mode-syntax-table)
  (setq local-abbrev-table sml-mode-abbrev-table)
  ;; A paragraph is separated by blank lines or ^L only.
  
  (set (make-local-variable 'paragraph-start)
       (concat "^[\t ]*$\\|" page-delimiter))
  (set (make-local-variable 'paragraph-separate) paragraph-start)
  (set (make-local-variable 'indent-line-function) 'sml-indent-line)
  (set (make-local-variable 'comment-start) "(* ")
  (set (make-local-variable 'comment-end) " *)")
  (set (make-local-variable 'comment-nested) t)
  ;;(set (make-local-variable 'block-comment-start) "* ")
  ;;(set (make-local-variable 'block-comment-end) "")
  (set (make-local-variable 'comment-column) 40)
  (set (make-local-variable 'comment-start-skip) "(\\*+\\s-*")
  (set (make-local-variable 'comment-indent-function) 'sml-comment-indent))

(defun sml-electric-pipe ()
  "Insert a \"|\".
Depending on the context insert the name of function, a \"=>\" etc."
  (interactive)
  (sml-with-ist
   (unless (save-excursion (skip-chars-backward "\t ") (bolp)) (insert "\n"))
   (insert "| ")
   (let ((text
	  (save-excursion
	    (backward-char 2)		;back over the just inserted "| "
	    (let ((sym (sml-find-matching-starter sml-pipeheads
						  (sml-op-prec "|" 'back))))
	      (sml-forward-sym)
	      (sml-forward-spaces)
	      (cond
	       ((string= sym "|")
		(let ((f (sml-forward-sym)))
		  (sml-find-forward "\\(=>\\|=\\||\\)\\S.")
		  (cond
		   ((looking-at "|") "") ;probably a datatype
		   ((looking-at "=>") " => ") ;`case', or `fn' or `handle'
		   ((looking-at "=") (concat f "  = "))))) ;a function
	       ((string= sym "and")
		;; could be a datatype or a function
		(while (and (setq sym (sml-forward-sym))
			    (string-match "^'" sym))
		  (sml-forward-spaces))
		(sml-forward-spaces)
		(if (or (not sym)
			(equal (sml-forward-sym) "d="))
		    ""
		  (concat sym "  = ")))
	       ;; trivial cases
	       ((string= sym "fun")
		(while (and (setq sym (sml-forward-sym))
			    (string-match "^'" sym))
		  (sml-forward-spaces))
		(concat sym "  = "))
	       ((member sym '("case" "handle" "fn" "of")) " => ")
	       ;;((member sym '("abstype" "datatype")) "")
	       (t ""))))))

     (insert text)
     (indent-according-to-mode)
     (beginning-of-line)
     (skip-chars-forward "\t |")
     (skip-syntax-forward "w")
     (skip-chars-forward "\t ")
     (when (= ?= (char-after)) (backward-char)))))

(defun sml-electric-semi ()
  "Insert a \;.
If variable `sml-electric-semi-mode' is t, indent the current line, insert
a newline, and indent."
  (interactive)
  (insert "\;")
  (if sml-electric-semi-mode
      (reindent-then-newline-and-indent)))

;;; INDENTATION !!!

(defun sml-mark-function ()
  "Synonym for `mark-paragraph' -- sorry.
If anyone has a good algorithm for this..."
  (interactive)
  (mark-paragraph))

;; (defun sml-indent-region (begin end)
;;   "Indent region of ML code."
;;   (interactive "r")
;;   (message "Indenting region...")
;;   (save-excursion
;;     (goto-char end) (setq end (point-marker)) (goto-char begin)
;;     (while (< (point) end)
;;       (skip-chars-forward "\t\n ")
;;       (indent-according-to-mode)
;;       (end-of-line))
;;     (move-marker end nil))
;;   (message "Indenting region... done"))

(defun sml-indent-line ()
  "Indent current line of ML code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
	(indent (or (ignore-errors (sml-calculate-indentation)) 0)))
    (if savep
	(save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun sml-back-to-outer-indent ()
  "Unindents to the next outer level of indentation."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward "\t ")
    (let ((start-column (current-column))
          (indent (current-column)))
      (if (> start-column 0)
          (progn
            (save-excursion
              (while (>= indent start-column)
                (if (re-search-backward "^[^\n]" nil t)
                    (setq indent (current-indentation))
                  (setq indent 0))))
            (backward-delete-char-untabify (- start-column indent)))))))

(defun sml-find-comment-indent ()
  (save-excursion
    (let ((depth 1))
      (while (> depth 0)
	(if (re-search-backward "(\\*\\|\\*)" nil t)
	    (cond
	     ((looking-at "*)") (incf depth))
	     ((looking-at comment-start-skip) (decf depth)))
	  (setq depth -1)))
      (if (= depth 0)
	  (1+ (current-column))
	nil))))

(defun sml-calculate-indentation ()
  (save-excursion
    (beginning-of-line) (skip-chars-forward "\t ")
    (sml-with-ist
     ;; Indentation for comments alone on a line, matches the
     ;; proper indentation of the next line.
     (when (looking-at "(\\*") (sml-forward-spaces))
     (let (data
	   (sml-point (point))
	   (sym (save-excursion (sml-forward-sym))))
       (or
	;; allow the user to override the indentation
	(when (looking-at (concat ".*" (regexp-quote comment-start)
				  "[ \t]*fixindent[ \t]*"
				  (regexp-quote comment-end)))
	  (current-indentation))

	;; continued comment
	(and (looking-at "\\*") (sml-find-comment-indent))

	;; Continued string ? (Added 890113 lbn)
	(and (looking-at "\\\\")
	     (save-excursion
	       (if (save-excursion (previous-line 1)
				   (beginning-of-line)
				   (looking-at "[\t ]*\\\\"))
		   (progn (previous-line 1) (current-indentation))
		 (if (re-search-backward "[^\\\\]\"" nil t)
		     (1+ (current-column))
		   0))))

	(and (setq data (assoc sym sml-close-paren))
	     (sml-indent-relative sym data))

	(and (member (save-excursion (sml-forward-sym)) sml-starters-syms)
	     (let ((sym (unless (save-excursion (sml-backward-arg))
			  (sml-backward-spaces)
			  (sml-backward-sym))))
	       (if sym (sml-get-sym-indent sym)
		 ;; FIXME: this can take a *long* time !!
		 (sml-find-matching-starter sml-starters-syms)
		 (current-column))))

	(and (string= sym "|") (sml-indent-pipe))

	(sml-indent-arg)
	(sml-indent-default))))))

(defun sml-indent-relative (sym data)
  (save-excursion
    (sml-forward-sym) (sml-backward-sexp nil)
    (unless (second data) (sml-backward-spaces) (sml-backward-sym))
    (+ (or (cdr (assoc sym sml-symbol-indent)) 0)
       (sml-delegated-indent))))

(defun sml-indent-pipe ()
  (let ((sym (sml-find-matching-starter sml-pipeheads
					(sml-op-prec "|" 'back))))
    (when sym
      (if (string= sym "|")
	  (if (sml-bolp) (current-column) (sml-indent-pipe))
	(let ((pipe-indent (or (cdr (assoc "|" sml-symbol-indent)) -2)))
	  (when (member sym '("datatype" "abstype"))
	    (re-search-forward "="))
	  (sml-forward-sym)
	  (sml-forward-spaces)
	  (+ pipe-indent (current-column)))))))

(defun sml-find-forward (re)
  (sml-forward-spaces)
  (while (and (not (looking-at re))
	      (progn
		(or (ignore-errors (forward-sexp 1) t) (forward-char 1))
		(sml-forward-spaces)
		(not (looking-at re))))))

(defun sml-indent-arg ()
  (and (save-excursion (ignore-errors (sml-forward-arg)))
       ;;(not (looking-at sml-not-arg-re))
       ;; looks like a function or an argument
       (sml-move-if (sml-backward-arg))
       ;; an argument
       (if (save-excursion (not (sml-backward-arg)))
	   ;; a first argument
	   (+ (current-column) sml-indent-args)
	 ;; not a first arg
	 (while (and (/= (current-column) (current-indentation))
		     (sml-move-if (sml-backward-arg))))
	 (unless (save-excursion (sml-backward-arg))
	   ;; all earlier args are on the same line
	   (sml-forward-arg) (sml-forward-spaces))
	 (current-column))))

(defun sml-get-indent (data sym)
  (let ((head-sym (pop data)) d)
    (cond
     ((not (listp data)) data)
     ((setq d (member sym data)) (second d))
     ((and (consp data) (not (stringp (car data)))) (car data))
     (t sml-indent-level))))

(defun sml-dangling-sym ()
  (save-excursion
    (and (not (sml-bolp))
	 (< (sml-point-after (end-of-line))
	    (sml-point-after (sml-forward-sym)
			     (sml-forward-spaces))))))

(defun sml-delegated-indent ()
  (if (sml-dangling-sym)
      (sml-indent-default 'noindent)
    (sml-move-if (backward-word 1)
		 (looking-at sml-agglomerate-re))
    (current-column)))

(defun sml-get-sym-indent (sym &optional style)
  "Find the indentation for the SYM we're `looking-at'.
If indentation is delegated, the point will be at the start of
the parent at the end of this function.
Optional argument STYLE is currently ignored"
  (assert (equal sym (save-excursion (sml-forward-sym))))
  (save-excursion
    (let ((delegate (assoc sym sml-close-paren))
	  (head-sym sym))
      (when (and delegate (not (eval (third delegate))))
	;;(sml-find-match-backward sym delegate)
	(sml-forward-sym) (sml-backward-sexp nil)
	(setq head-sym
	      (if (second delegate)
		  (save-excursion (sml-forward-sym))
		(sml-backward-spaces) (sml-backward-sym))))

      (let ((idata (assoc head-sym sml-indent-rule)))
	(when idata
	  ;;(if (or style (not delegate))
	  ;; normal indentation
	  (let ((indent (sml-get-indent idata sym)))
	    (when indent (+ (sml-delegated-indent) indent)))
	  ;; delgate indentation to the parent
	  ;;(sml-forward-sym) (sml-backward-sexp nil)
	  ;;(let* ((parent-sym (save-excursion (sml-forward-sym)))
	  ;;     (parent-indent (cdr (assoc parent-sym sml-indent-starters))))
	  ;; check the special rules
	  ;;(+ (sml-delegated-indent)
	  ;; (or (sml-get-indent indent-data 1 'strict)
	  ;; (sml-get-indent parent-indent 1 'strict)
	  ;; (sml-get-indent indent-data 0)
	  ;; (sml-get-indent parent-indent 0))))))))
	  )))))

(defun sml-indent-default (&optional noindent)
  (let* ((sym-after (save-excursion (sml-forward-sym)))
	 (_ (sml-backward-spaces))
	 (sym-before (sml-backward-sym))
	 (sym-indent (and sym-before (sml-get-sym-indent sym-before))))
    (if sym-indent
	;; the previous sym is an indentation introducer: follow the rule
	(let ((indent-after (or (cdr (assoc sym-after sml-symbol-indent)) 0)))
	  (if noindent
	      ;;(current-column)
	      sym-indent
	    (+ sym-indent indent-after)))
      ;; default-default
      (let* ((prec-after (sml-op-prec sym-after 'back))
	     (prec (or (sml-op-prec sym-before 'back) prec-after 100)))
	;; go back until you hit a symbol that has a lower prec than the
	;; "current one", or until you backed over a sym that has the same prec
	;; but is at the beginning of a line.
	(while (and (not (sml-bolp))
		    (sml-move-if (sml-backward-sexp (1- prec)))
		    (not (sml-bolp)))
	  (while (sml-move-if (sml-backward-sexp prec))))
	;; the `noindent' case does back over an introductory symbol
	;; such as `fun', ...
	(when noindent
	  (sml-move-if
	   (sml-backward-spaces)
	   (member (sml-backward-sym) sml-starters-syms)))
	(current-column)))))


(defun sml-bolp ()
  (save-excursion
    (skip-chars-backward " \t|") (bolp)))


;; maybe `|' should be set to word-syntax in our temp syntax table ?
(defun sml-current-indentation ()
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t|")
    (current-column)))


(defun sml-find-matching-starter (syms &optional prec)
  (let (sym)
    (ignore-errors
      (while
	  (progn (sml-backward-sexp prec)
		 (setq sym (save-excursion (sml-forward-sym)))
		 (not (or (member sym syms) (bobp)))))
      (unless (bobp) sym))))

(defun sml-comment-indent ()
  (if (looking-at "^(\\*")              ; Existing comment at beginning
      0                                 ; of line stays there.
    (save-excursion
      (skip-chars-backward " \t")
      (max (1+ (current-column))        ; Else indent at comment column
           comment-column))))           ; except leave at least one space.

;;; INSERTING PROFORMAS (COMMON SML-FORMS)

(defvar sml-forms-alist nil
  "*Alist of code templates.
You can extend this alist to your heart's content.  For each additional
template NAME in the list, declare a keyboard macro or function (or
interactive command) called 'sml-form-NAME'.
If 'sml-form-NAME' is a function it takes no arguments and should
insert the template at point\; if this is a command it may accept any
sensible interactive call arguments\; keyboard macros can't take
arguments at all.  Apropos keyboard macros, see `name-last-kbd-macro'
and `sml-addto-forms-alist'.
`sml-forms-alist' understands let, local, case, abstype, datatype,
signature, structure, and functor by default.")

(defmacro sml-def-skeleton (name interactor &rest elements)
  (let ((fsym (intern (concat "sml-form-" name))))
    `(progn
       (add-to-list 'sml-forms-alist ',(cons name fsym))
       (define-skeleton ,fsym
	 ,(format "SML-mode skeleton for `%s..' expressions" name)
	 ,interactor
	 ,(concat name " ") >
	 ,@elements))))
(put 'sml-def-skeleton 'lisp-indent-function 2)

(sml-def-skeleton "let" nil
  _ "\nin" > "\nend" >)

(sml-def-skeleton "if" nil
  _ " then " > "\nelse " >)

(sml-def-skeleton "local" nil
  _ "\nin" > "\nend" >)

(sml-def-skeleton "case" "Case expr: "
  str "\nof " > _ " => ")

(sml-def-skeleton "signature" "Signature name: "
  str " =\nsig" > "\n" > _ "\nend" >)

(sml-def-skeleton "structure" "Structure name: "
  str " =\nstruct" > "\n" > _ "\nend" >)

(sml-def-skeleton "functor" "Functor name: "
  str " () : =\nstruct" > "\n" > _ "\nend" >)

(sml-def-skeleton "datatype" "Datatype name and type parameters: "
  str " =" \n)

(sml-def-skeleton "abstype" "Abstype name and type parameters: "
  str " =" \n _ "\nwith" > "\nend" >)

;;

(defun sml-forms-menu (menu)
  (easy-menu-filter-return
   (easy-menu-create-menu "Forms"
	 (mapcar (lambda (x)
		   (let ((name (car x))
			 (fsym (cdr x)))
		     (vector name fsym t)))
		 sml-forms-alist))))

(defvar sml-last-form "let")

(defun sml-electric-space ()
  "Expand a symbol into an SML form, or just insert a space.
If the point directly precedes a symbol for which an SML form exists,
the corresponding form is inserted."
  (interactive)
  (let* ((point (point))
	 (sym (sml-backward-sym)))
    (if (not (and sym (assoc sym sml-forms-alist)))
	(progn (goto-char point) (insert " "))
      (delete-region (point) point)
      (sml-insert-form sym nil))))

(defun sml-insert-form (name newline)
  "Interactive short-cut to insert the NAME common ML form.
If a prefix argument is given insert a NEWLINE and indent first, or
just move to the proper indentation if the line is blank\; otherwise
insert at point (which forces indentation to current column).

The default form to insert is 'whatever you inserted last time'
\(just hit return when prompted\)\; otherwise the command reads with
completion from `sml-forms-alist'."
  (interactive
   (list (completing-read
	  (format "Form to insert: (default %s) " sml-last-form)
	  sml-forms-alist nil t nil)
	 current-prefix-arg))
  ;; default is whatever the last insert was...
  (if (string= name "") (setq name sml-last-form) (setq sml-last-form name))
  (unless (or (not newline)
	      (save-excursion (beginning-of-line) (looking-at "\\s-*$")))
    (insert "\n"))
  (unless (/= ?w (char-syntax (char-before))) (insert " "))
  (let ((f (cdr (assoc name sml-forms-alist))))
    (cond
     ((commandp f) (command-execute f))
     (f (funcall f))
     (t (error "Undefined form: %s" name)))))

;; See also macros.el in emacs lisp dir.

(defun sml-addto-forms-alist (name)
  "Assign a name to the last keyboard macro defined.
Argument NAME is transmogrified to sml-form-NAME which is the symbol
actually defined.

The symbol's function definition becomes the keyboard macro string.

If that works, NAME is added to `sml-forms-alist' so you'll be able to
reinvoke the macro through \\[sml-insert-form].  You might want to save
the macro to use in a later editing session -- see `insert-kbd-macro'
and add these macros to your .emacs file.

See also `edit-kbd-macro' which is bound to \\[edit-kbd-macro]."
  (interactive "sName for last kbd macro (\"sml-form-\" will be added): ")
  (when (string= name "") (error "No command name given"))
  (let ((fsym (intern (concat "sml-form-" name))))
    (name-last-kbd-macro fsym)
    (message "Macro bound to %s" fsym)
    (add-to-list 'sml-forms-alist (cons name fsym))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;  SML/NJ's Compilation Manager support  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(add-to-list 'completion-ignored-extensions "CM/")
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cm\\'" . sml-cm-mode))
;;;###autoload
(define-generic-mode 'sml-cm-mode
  '(("(*" . "*)"))
  '("library" "Library" "LIBRARY" "group" "Group" "GROUP" "is" "IS"
    "structure" "functor" "signature" "funsig")
  nil '("\\.cm\\'")
  (list (lambda () (local-set-key "\C-c\C-c" 'sml-compile)))
  "Generic mode for SML/NJ's Compilation Manager configuration files.")


(provide 'sml-mode)

;;; sml-mode.el ends here
