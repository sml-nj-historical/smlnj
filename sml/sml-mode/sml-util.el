;;; sml-util.el

(defconst rcsid-sml-util "@(#)$Name$:$Id$")

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
(require 'sml-compat)

;;

(defmacro concatq (&rest ss)
  "Concatenate all the arguments and make the result a string.
As opposed to `concat', `concatq' does not evaluate its arguments
and is hence executed at macro-expansion-time."
  (apply 'concat ss))

(defun flatten (ls &optional acc)
  (if (null ls) acc
    (let ((rest (flatten (cdr ls) acc))
	  (head (car ls)))
      (if (listp head)
	  (flatten head rest)
	(cons head rest)))))

(defun custom-create-map (m bs args)
  (unless (keymapp m)
    (setq bs (append m bs))
    (setq m (make-sparse-keymap)))
  (dolist (b bs)
    (let ((key (car b))
	  (binding (cdr b)))
      (cond
       ((symbolp key)
	(substitute-key-definition key binding m global-map))
       ((not (lookup-key m key))
	(define-key m key binding)))))
  (while args
    (let ((key (first args))
	  (val (second args)))
      (cond
       ((eq key :inherit)
	(cond
	 ((keymapp val) (set-keymap-parent m val))
	 (t (set-keymap-parents m val))))
       (t (error "Uknown argument %s in defmap" key))))
    (setq args (cddr args))))

(defmacro defmap (m bs doc &rest args)
  `(progn
     (defvar ,m (make-sparse-keymap) ,doc)
     (custom-create-map ,m ,bs ,(cons 'list args))))

(defmacro defsyntax (st css doc &rest args)
  `(defvar ,st
     (let ((st (make-syntax-table ,(cadr (memq :copy args)))))
       (dolist (cs ,css)
	 (let ((char (car cs))
	       (syntax (cdr cs)))
	   (if (sequencep char)
	       (mapcar* (lambda (c) (modify-syntax-entry c syntax st))
			char)
	     (modify-syntax-entry char syntax st))))
       st)
     doc))

;;
(provide 'sml-util)
