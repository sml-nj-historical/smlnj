;;; sml-move.el

(defconst rcsid-sml-defs "@(#)$Name$:$Id$")

;; Copyright (C) 1999-1999  Stefan Monnier <monnier@cs.yale.edu>
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

(require 'cl)
(require 'sml-util)

;;; 
;;; Code
;;; 

(defvar sml-outline-regexp "[ \t]*\\((\\*+\\|\\(let[ \t]+\\)?fun.\\)"
  "Regexp matching a major heading.")

;;; 
;;; Internal defines
;;; 

(defmap sml-bindings
  ;; smarter cursor movement
  '((forward-sexp	. sml-user-forward-sexp)
    (backward-sexp	. sml-user-backward-sexp)
    ;; Text-formatting commands:
    ("\C-c\C-m" . sml-insert-form)
    ("\C-c\C-i" . sml-mode-info)
    ("\M-|"     . sml-electric-pipe)
    ("\;"       . sml-electric-semi)
    ("\M-\t"    . sml-back-to-outer-indent)
    ("\C-\M-\\" . sml-indent-region)
    ("\t"       . sml-indent-line)	; ...except this one
    ;; Process commands added to sml-mode-map -- these should autoload
    ("\C-c\C-l" . sml-load-file)
    ("\C-c`"    . sml-next-error))
  "Generic bindings used in sml-mode and sml-inferior-mode.")

(defmap sml-mode-map
  '(("\C-c\C-c" . sml-make)
    ("\C-c\C-s" . switch-to-sml)
    ("\C-c\C-r" . sml-send-region)
    ("\C-c\C-b" . sml-send-buffer))
  "The keymap used in sml-mode."
  :inherit sml-bindings)

(defsyntax sml-mode-syntax-table 
  '((?\*   . ". 23n")
    (?\(   . "()1")
    (?\)   . ")(4")
    ("._'" . "_")
    (",;"  . ".")
    ;; `!' is not really a prefix-char, oh well!
    ("~#!" . "'")
    ("%&$+-/:<=>?@`^|"	 . "."))
  "The syntax table used in sml-mode.")

;;
;; regexps
;;

(defun sml-syms-re (&rest syms)
  (concat "\\<" (regexp-opt (flatten syms) t) "\\>"))

;;

(defconst sml-module-head-syms
  '("signature" "structure" "functor" "abstraction"))

(defconst sml-begin-symbols-re
  (sml-syms-re "let" "abstype" "local" "struct" "sig")
  "Symbols matching the `end' symbol.")

;; (defconst sml-user-begin-symbols-re
;;   (sml-syms-re "let" "abstype" "local" "struct" "sig" "in" "with")
;;   "Symbols matching (loosely) the `end' symbol.")

(defconst sml-sexp-head-symbols-re
  (sml-syms-re "let" "abstype" "local" "struct" "sig" "in" "with"
	       "if" "then" "else" "case" "of" "fn" "fun" "val" "and"
	       sml-module-head-syms
	       "handle" "raise")
  "Symbols starting an sexp.")

;; (defconst sml-not-arg-start-re
;;   (sml-syms-re "in" "of" "end" "andalso")
;;   "Symbols that can't be found at the head of an arg.")

;; (defconst sml-not-arg-re
;;   (sml-syms-re "in" "of" "end" "andalso")
;;   "Symbols that should not be confused with an arg.")

(defconst sml-indent-starters
  (list
   (cons "\\<struct\\>" 0)
   (cons (sml-syms-re sml-module-head-syms) '(sml-indent-level 0))
   (cons "\\<local\\>" '(sml-indent-level 0))
   (cons "\\<of\\>" '(3 nil))
   (cons "\\<else\\>" '(sml-indent-level 0))
   (cons "\\<in\\>" '(sml-indent-level nil))
   (cons (sml-syms-re "abstype" "and" "case" "of" "datatype"
		      "fun" "if" "then" "else" "sharing" "infix" "infixr"
		      "let" "in" "local" "nonfix" "open" "raise" "sig"
		      "struct" "type" "val" "while" "do" "with" "withtype")
	 'sml-indent-level))
  "")

(defconst sml-starters-indent-after
  (sml-syms-re "let" "local" "struct" "in" "sig" "with")
  "Indent after these.")

(defconst sml-=-starter-re
  (sml-syms-re "val" "fun" "and" "datatype" "type" "abstype" "eqtype"
	       sml-module-head-syms)
  "keywords which can be followed by a `='")

(defconst sml-delegate
  (list
   (cons (sml-syms-re "of" "else" "then") '(not (sml-bolp)))
   (cons "\\<in\\>" t))
  "Words which might delegate indentation to their parent.")

(defconst sml-starters-syms
  (append sml-module-head-syms
	  '("abstype" "datatype" "exception" "fun"
	    "local" "infix" "infixr" "sharing" "nonfix"
	    "open" "type" "val" "and"
	    "withtype" "with"))
  "The starters of new expressions.")
(defconst sml-starters-re (sml-syms-re sml-starters-syms))

(defconst sml-exptrail-syms
  '("if" "then" "else" "while" "do" "case" "of" "raise" "fn"))

(defconst sml-pipehead-re
  (sml-syms-re "fun" "fn" "and" "handle" "case" "datatype" "abstype")
  "A `|' corresponds to one of these.")

;;
(provide 'sml-defs)
