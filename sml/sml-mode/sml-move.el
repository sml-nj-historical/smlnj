;;; sml-move.el

(defconst rcsid-sml-move "@(#)$Name$:$Id$")

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
(require 'sml-defs)

;;

(defsyntax sml-internal-syntax-table
  '((?_  . "w")
    (?'  . "w")
    (?.  . "w")
    ;; treating `~' as a word constituent is not quite right, but
    ;; close enough.  Think about 12.3E~2 for example.  Also `~' on its
    ;; own *is* a nonfix symbol.
    (?~  . "w"))
  "Syntax table used for internal sml-mode operation."
  :copy sml-mode-syntax-table)

(defun sml-op-prec (op dir)
  "return the precedence of OP or nil if it's not an infix.
DIR should be set to BACK if you want to precedence w.r.t the left side
    and to FORW for the precedence w.r.t the right side.
This assumes that we are looking-at the OP."
  (cond
   ((not op) nil)
   ;;((or (string-match (sml-syms-re (appen
   ((or (string-equal ";" op) (string-equal "," op)) 10)
   ((or (string-equal "=>" op)
	(and (string-equal "=" op)
	     ;; not the polymorphic equlity
	     (> (sml-point-after (re-search-backward sml-=-starter-re nil 'top))
		(sml-point-after (re-search-backward "=" nil 'top)))))
    ;; depending on the direction
    (if (eq dir 'back) 65 40))
   ((or (string-match (sml-syms-re "case" "of" "fn") op)) 45)
   ((or (string-equal "|" op)) (if (eq dir 'back) 47 30))
   ((or (string-match (sml-syms-re "if" "then" "else" "while" "do" "raise") op)) 50)
   ((or (string-equal "handle" op)) 60)
   ((or (string-equal "orelse" op)) 70)
   ((or (string-equal "andalso" op)) 80)
   ((or (string-equal ":" op) (string-equal ":>" op)) 90)
   ((or (string-equal "->" op)) 95)
   ;; standard infix ops: 10*(10 + prec) as defined in `the definition of SML'
   ((or (string-equal "!" op)) nil)
   ((or (string-equal "~" op)) nil)
   ((or (string-equal ":=" op)) 130)
   ((or (string-match "\\`[<>]?=?\\'" op)) 140)
   ((or (string-equal "::" op)) 150)
   ((or (string-equal "+" op) (string-equal "-" op)) 160)
   ((or (string-equal "/" op) (string-equal "*" op)
	(string-equal "div" op) (string-equal "mod" op)) 170)
   ;; default heuristic: alphanum symbols are not infix
   ((or (string-match "\\sw" op)) nil)
   (t 100)))


(defmacro sml-with-ist (&rest r)
  `(let ((sml-ost (syntax-table))
	 (case-fold-search nil))
     (unwind-protect
	 (progn (set-syntax-table sml-internal-syntax-table) . ,r)
       (set-syntax-table sml-ost))))
(def-edebug-spec sml-with-ist t)

(defmacro sml-move-if (f &optional c)
  `(let* ((-sml-move-if-pt (point))
	  (-sml-move-if-res ,f))
     (or ,(or c '-sml-move-if-res) (progn (goto-char -sml-move-if-pt) nil))))
(def-edebug-spec sml-move-if t)

(defmacro sml-move-read (&rest body)
  `(let ((-sml-move-read-pt (point)))
     ,@body
     (when (/= (point) -sml-move-read-pt)
       (buffer-substring (point) -sml-move-read-pt))))
(def-edebug-spec sml-move-read t)

(defmacro sml-point-after (&rest body)
  `(save-excursion
     ,@body
     (point)))
(def-edebug-spec sml-point-after t)

;;

(defun sml-forward-spaces ()
  (let ((parse-sexp-lookup-properties t))
    (forward-comment 100000)))


(defun sml-looking-back-at (re)
  (save-excursion
    (when (= 0 (skip-syntax-backward "w")) (backward-char))
    (looking-at re)))

;;
;; moving forward around sexps
;;

(defun sml-find-match-forward (this match)
  "Only works for word matches"
  (let ((case-fold-search nil)
	(parse-sexp-lookup-properties t)
	(parse-sexp-ignore-comments t)
	(level 1)
	(either (concat this "\\|" match)))
    (while (> level 0)
      (forward-sexp 1)
      (while (not (or (eobp) (sml-looking-back-at either)))
	(condition-case () (forward-sexp 1) (error (forward-char 1))))
      (setq level
	    (cond
	     ((sml-looking-back-at this) (1+ level))
	     ((sml-looking-back-at match) (1- level))
	     (t (error "Unbalanced")))))
    t))

;; (defun sml-forward-sexp (&optional count strict)
;;   "Moves one sexp forward if possible, or one char else.
;; Returns T if the move indeed moved through one sexp and NIL if not."
;;   (let ((parse-sexp-lookup-properties t)
;; 	(parse-sexp-ignore-comments t))
;;     (condition-case ()
;; 	(progn
;; 	  (forward-sexp 1)
;; 	  (cond
;; 	   ((sml-looking-back-at
;; 	     (if strict sml-begin-symbols-re sml-user-begin-symbols-re))
;; 	    (sml-find-match-forward sml-begin-symbols-re "\\<end\\>") t)
;; 	   ((sml-looking-back-at "\\<end\\>") nil)
;; 	   (t t)))
;;       (error (forward-char 1) nil))))

;; the terminators should be chosen more carefully:
;; `let' isn't one while `=' may be
;; (defun sml-forward-sexps (&optional end)
;;   (sml-forward-sexp)
;;   (while (not (sml-looking-back-at (or end (concat sml-keywords-regexp "\\|[])}|:;]"))))
;;       (sml-forward-sexp)))

;;
;; now backwards
;;

(defun sml-backward-spaces ()
  (let ((parse-sexp-lookup-properties t))
    (forward-comment -100000)))

(defun sml-find-match-backward (this match)
  (let ((parse-sexp-lookup-properties t)
	(parse-sexp-ignore-comments t)
	(level 1)
	(either (concat this "\\|" match)))
    (while (> level 0)
      (backward-sexp 1)
      (while (not (or (bobp) (looking-at either)))
	(condition-case () (backward-sexp 1) (error (backward-char 1))))
      (setq level
	    (cond
	     ((looking-at this) (1+ level))
	     ((looking-at match) (1- level))
	     (t (error "Unbalanced")))))
    t))

(defun sml-forward-sym ()
  (or (/= 0 (skip-syntax-forward ".'"))
      (/= 0 (skip-syntax-forward "'w_"))))

(defun sml-backward-sym ()
  (or (/= 0 (skip-syntax-backward ".'"))
      (/= 0 (skip-syntax-backward "'w_"))))

(defun sml-backward-sexp (prec)
  "Moves one sexp backward if possible, or one char else.
Returns T if the move indeed moved through one sexp and NIL if not."
  (let ((parse-sexp-lookup-properties t)
	(parse-sexp-ignore-comments t))
    (sml-backward-spaces)
    (let* ((point (point))
	   (op (sml-move-read (sml-backward-sym)))
	   (op-prec (sml-op-prec op 'back)))
      (cond
       ((not op)
	(let ((point (point)))
	  (ignore-errors (backward-sexp 1))
	  (if (/= point (point)) t (backward-char 1) nil)))
       ;; let...end atoms
       ((or (string-equal "end" op)
	    (and (not prec)
		 (or (string-equal "in" op) (string-equal "with" op))))
	(sml-find-match-backward "\\<end\\>" sml-begin-symbols-re))
       ;; don't forget the `op' special keyword
       ((sml-move-if (progn (sml-backward-spaces) (skip-syntax-backward "w_"))
		     (looking-at "\\<op\\>")) t)
       ;; special rules for nested constructs like if..then..else
       ((and (or (not prec) (and prec op-prec (< prec op-prec)))
	     (string-match (sml-syms-re sml-exptrail-syms) op))
	(cond
	 ((or (string-equal "else" op) (string-equal "then" op))
	  (sml-find-match-backward "\\<else\\>" "\\<if\\>"))
	 ((string-equal "of" op)
	  (sml-find-match-backward "\\<of\\>" "\\<case\\>"))
	 ((string-equal "do" op)
	  (sml-find-match-backward "\\<do\\>" "\\<while\\>"))
	 (t prec)))
       ;; infix ops precedence
       ((and prec op-prec) (< prec op-prec))
       ;; [ prec = nil ]  a new operator, let's skip the sexps until the next
       (op-prec (while (sml-move-if (sml-backward-sexp op-prec))) t)
       ;; special symbols indicating we're getting out of a nesting level
       ((string-match sml-sexp-head-symbols-re op) nil)
       ;; if the op was not alphanum, then we still have to do the backward-sexp
       ;; this reproduces the usual backward-sexp, but it might be bogus
       ;; in this case since !@$% is a perfectly fine symbol
       (t t))))) ;(or (string-match "\\sw" op) (sml-backward-sexp prec))

(defun sml-forward-sexp (prec)
  "Moves one sexp forward if possible, or one char else.
Returns T if the move indeed moved through one sexp and NIL if not."
  (let ((parse-sexp-lookup-properties t)
	(parse-sexp-ignore-comments t))
    (sml-forward-spaces)
    (let* ((point (point))
	   (op (sml-move-read (sml-forward-sym)))
	   (op-prec (sml-op-prec op 'forw)))
      (cond
       ((not op)
	(let ((point (point)))
	  (ignore-errors (forward-sexp 1))
	  (if (/= point (point)) t (forward-char 1) nil)))
       ;; let...end atoms
       ((or (string-match sml-begin-symbols-re op)
	    (and (not prec)
		 (or (string-equal "in" op) (string-equal "with" op))))
	(sml-find-match-forward sml-begin-symbols-re "\\<end\\>"))
       ;; don't forget the `op' special keyword
       ((string-equal "op" op) (sml-forward-sym))
       ;; infix ops precedence
       ((and prec op-prec) (< prec op-prec))
       ;; [ prec = nil ]  if...then...else
       ;; ((or (string-equal "else" op) (string-equal "then" op))
       ;;  (sml-find-match-backward "\\<else\\>" "\\<if\\>"))
       ;; [ prec = nil ]  case...of
       ;; ((string-equal "of" op)
       ;;  (sml-find-match-backward "\\<of\\>" "\\<case\\>"))
       ;; [ prec = nil ]  while...do
       ;; ((string-equal "do" op)
       ;;  (sml-find-match-backward "\\<do\\>" "\\<while\\>"))
       ;; [ prec = nil ]  a new operator, let's skip the sexps until the next
       (op-prec (while (sml-move-if (sml-forward-sexp op-prec))) t)
       ;; special symbols indicating we're getting out of a nesting level
       ((string-match sml-sexp-head-symbols-re op) nil)
       ;; if the op was not alphanum, then we still have to do the backward-sexp
       ;; this reproduces the usual backward-sexp, but it might be bogus
       ;; in this case since !@$% is a perfectly fine symbol
       (t t))))) ;(or (string-match "\\sw" op) (sml-backward-sexp prec))

(defun sml-in-word-p ()
  (and (eq ?w (char-syntax (char-before)))
       (eq ?w (char-syntax (char-after)))))

(defun sml-user-backward-sexp (&optional count)
  "Like `backward-sexp' but tailored to the SML syntax."
  (interactive "p")
  (unless count (setq count 1))
  (sml-with-ist
   (let ((point (point)))
     (if (< count 0) (sml-user-forward-sexp (- count))
       (when (sml-in-word-p) (forward-word 1))
       (dotimes (i count)
	 (unless (sml-backward-sexp nil)
	   (goto-char point)
	   (error "Containing expression ends prematurely")))))))

(defun sml-user-forward-sexp (&optional count)
  "Like `forward-sexp' but tailored to the SML syntax."
  (interactive "p")
  (unless count (setq count 1))
  (sml-with-ist
   (let ((point (point)))
     (if (< count 0) (sml-user-backward-sexp (- count))
       (when (sml-in-word-p) (backward-word 1))
       (dotimes (i count)
	 (unless (sml-forward-sexp nil)
	   (goto-char point)
	   (error "Containing expression ends prematurely")))))))

;;(defun sml-forward-thing ()
;;  (if (= ?w (char-syntax (char-after))) (forward-word 1) (forward-char 1)))

(defun sml-backward-arg () (sml-backward-sexp 1000))
(defun sml-forward-arg () (sml-forward-sexp 1000))

;; (defun sml-backward-arg ()
;;   "Moves one sexp backward (and return T) if it is an argument."
;;   (let* ((point (point))
;; 	 (argp (and (sml-backward-sexp t)
;; 		    (not (looking-at sml-not-arg-re))
;; 		    (save-excursion
;; 		      (sml-forward-sexp 1 t)
;; 		      (sml-forward-spaces)
;; 		      (>= (point) point)))))
;;     (unless argp (goto-char point))
;;     argp))

;; (defun sml-backward-sexps (&optional end)
;;   (sml-backward-spaces)
;;   (let ((eos (point)))
;;     (sml-backward-sexp t)
;;     (while (not (save-restriction
;; 		  (narrow-to-region (point) eos)
;; 		  (looking-at (or end sml-keywords-regexp))))
;;       (sml-backward-spaces)
;;       (setq eos (point))
;;       (sml-backward-sexp t))
;;     (if (looking-at "\\sw")
;; 	(forward-word 1)
;;       (forward-char))
;;     (sml-forward-spaces)))

;; (defun sml-up-list ()
;;   (save-excursion
;;     (condition-case ()
;;         (progn
;;           (up-list 1)
;;           (point))
;;       (error 0))))

;;
(provide 'sml-move)
