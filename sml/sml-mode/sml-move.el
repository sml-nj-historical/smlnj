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

;;; 
;;; various macros
;;; 

(defmacro sml-with-ist (&rest r)
  (let ((ost-sym (make-symbol "oldtable")))
    `(let ((,ost-sym (syntax-table))
	   (case-fold-search nil)
	   (parse-sexp-lookup-properties t)
	   (parse-sexp-ignore-comments t))
       (unwind-protect
	   (progn (set-syntax-table sml-internal-syntax-table) . ,r)
	 (set-syntax-table ,ost-sym)))))
(def-edebug-spec sml-with-ist t)

(defmacro sml-move-if (f &optional c)
  (let ((pt-sym (make-symbol "point"))
	(res-sym (make-symbol "result")))
    `(let* ((,pt-sym (point))
	    (,res-sym ,f))
       (or ,(or c res-sym) (progn (goto-char ,pt-sym) nil)))))
(def-edebug-spec sml-move-if t)

(defmacro sml-point-after (&rest body)
  `(save-excursion
     ,@body
     (point)))
(def-edebug-spec sml-point-after t)

;;

(defun sml-preproc-alist (al)
  (reduce (lambda (x al)
	    (let ((k (car x))
		  (v (cdr x)))
	      (if (consp k)
		  (append (mapcar (lambda (y) (cons y v)) k) al)
		(cons x al))))
	  al
	  :initial-value nil
	  :from-end t))

(defvar sml-op-prec
  (sml-preproc-alist
   '(("before" . 0)
     ((":=" "o") . 3)
     ((">" ">=" "<>" "<" "<=" "=") . 4)
     (("::" "@") . 5)
     (("+" "-" "^") . 6)
     (("/" "*" "quot" "rem" "div" "mod") . 7)))
  "Alist of SML infix operators and their precedence.")

(defvar sml-syntax-prec
  (sml-preproc-alist
   '(((";" ",") . 10)
     ("|" . (47 . 30))
     (("case" "of" "fn") . 45)
     (("if" "then" "else" "while" "do" "raise") . 50)
     ("handle" . 60)
     ("orelse" . 70)
     ("andalso" . 80)
     ((":" ":>") . 90)
     ("->" . 95)))
  "Alist of pseudo-precedence of syntactic elements.")

(defun sml-op-prec (op dir)
  "return the precedence of OP or nil if it's not an infix.
DIR should be set to BACK if you want to precedence w.r.t the left side
    and to FORW for the precedence w.r.t the right side.
This assumes that we are looking-at the OP."
  (when op
    (let ((sprec (cdr (assoc op sml-syntax-prec))))
      (cond
       ((consp prec) (if (eq dir 'back) (car prec) (cdr prec)))
       (prec prec)

       ((or (string= "=>" op)
	    (and (string= "=" op)
	     ;; not the polymorphic equlity
	     (> (sml-point-after (re-search-backward sml-=-starter-re nil 'top))
		(sml-point-after (re-search-backward "=" nil 'top)))))
	;; depending on the direction
	(if (eq dir 'back) 65 40))

       (t
	(let ((prec (cdr (assoc op sml-op-prec))))
	  (when prec (+ prec 100))))))))

;;



(defun sml-forward-spaces () (forward-comment 100000))
(defun sml-backward-spaces () (forward-comment -100000))


;;
;; moving forward around matching symbols
;;

(defun sml-looking-back-at (re)
  (save-excursion
    (when (= 0 (skip-syntax-backward "w_")) (backward-char))
    (looking-at re)))

(defun sml-find-match-forward (this match)
  "Only works for word matches"
  (let ((level 1)
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

(defun sml-find-match-backward (this match)
  (let ((level 1)
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

;;; 
;;; read a symbol, including the special "op <sym>" case
;;; 

(defmacro sml-move-read (&rest body)
  (let ((pt-sym (make-symbol "point")))
    `(let ((,pt-sym (point)))
       ,@body
       (when (/= (point) ,pt-sym)
	 (buffer-substring (point) ,pt-sym)))))
(def-edebug-spec sml-move-read t)

(defun sml-forward-sym-1 ()
  (or (/= 0 (skip-syntax-forward ".'"))
      (/= 0 (skip-syntax-forward "'w_"))))
(defun sml-forward-sym ()
  (let ((sym (sml-move-read (sml-forward-sym-1))))
    (if (not (equal "op" sym)) sym
      (sml-forward-spaces)
      (concat "op " (or (sml-move-read (sml-forward-sym-1)) "")))))

(defun sml-backward-sym-1 ()
  (or (/= 0 (skip-syntax-backward ".'"))
      (/= 0 (skip-syntax-backward "'w_"))))
(defun sml-backward-sym ()
  (let ((sym (sml-move-read (sml-backward-sym-1))))
    (when sym
      ;; FIXME: what should we do if `sym' = "op" ?
      (let ((point (point)))
	(sml-backward-spaces)
	(if (equal "op" (sml-move-read (sml-backward-sym-1)))
	    (concat "op " sym)
	  (goto-char point)
	  sym)))))
    

(defun sml-backward-sexp (prec)
  "Moves one sexp backward if possible, or one char else.
Returns T if the move indeed moved through one sexp and NIL if not."
  (let ((parse-sexp-lookup-properties t)
	(parse-sexp-ignore-comments t))
    (sml-backward-spaces)
    (let* ((point (point))
	   (op (sml-backward-sym))
	   (op-prec (sml-op-prec op 'back)))
      (cond
       ((not op)
	(let ((point (point)))
	  (ignore-errors (backward-sexp 1))
	  (if (/= point (point)) t (backward-char 1) nil)))
       ;; let...end atoms
       ((or (string= "end" op)
	    (and (not prec)
		 (or (string= "in" op) (string= "with" op))))
	(sml-find-match-backward "\\<end\\>" sml-begin-symbols-re))
       ;; special rules for nested constructs like if..then..else
       ((and (or (not prec) (and prec op-prec (< prec op-prec)))
	     (string-match (sml-syms-re sml-exptrail-syms) op))
	(cond
	 ((or (string= "else" op) (string= "then" op))
	  (sml-find-match-backward "\\<else\\>" "\\<if\\>"))
	 ((string= "of" op)
	  (sml-find-match-backward "\\<of\\>" "\\<case\\>"))
	 ((string= "do" op)
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
	   (op (sml-forward-sym))
	   (op-prec (sml-op-prec op 'forw)))
      (cond
       ((not op)
	(let ((point (point)))
	  (ignore-errors (forward-sexp 1))
	  (if (/= point (point)) t (forward-char 1) nil)))
       ;; let...end atoms
       ((or (string-match sml-begin-symbols-re op)
	    (and (not prec)
		 (or (string= "in" op) (string= "with" op))))
	(sml-find-match-forward sml-begin-symbols-re "\\<end\\>"))
       ;; infix ops precedence
       ((and prec op-prec) (< prec op-prec))
       ;; [ prec = nil ]  if...then...else
       ;; ((or (string= "else" op) (string= "then" op))
       ;;  (sml-find-match-backward "\\<else\\>" "\\<if\\>"))
       ;; [ prec = nil ]  case...of
       ;; ((string= "of" op)
       ;;  (sml-find-match-backward "\\<of\\>" "\\<case\\>"))
       ;; [ prec = nil ]  while...do
       ;; ((string= "do" op)
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
  (and (eq ?w (char-syntax (or (char-before) ? )))
       (eq ?w (char-syntax (or (char-after) ? )))))

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

;;
(provide 'sml-move)
