;;; sml-nj.el: Modifies inferior-sml-mode defaults for SML/NJ.

;; Copyright (C) 1997, Matthew J. Morley

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

;;; DESCRIPTION

;; To use this library just put

;;(autoload 'sml-smlnj "sml-nj" "Set up and run SML/NJ." t)

;; in your .emacs file. If you only ever use the New Jersey compiler
;; then you might as well put something like

;;(setq sml-mode-hook
;;      '(lambda() "SML mode defaults to SML/NJ"
;;	 (define-key  sml-mode-map "\C-cp" 'sml-smlnj)))

;; for your sml-mode-hook. The command prompts for the program name 
;; and any command line options. 

;; If you need to reset the default value of sml-program-name, or any
;; of the other compiler variables, put something like

;;(eval-after-load "sml-nj" '(setq sml-program-name "whatever"))

;; in your .emacs -- or (better) you can use the inferior-sml-{load,
;; mode}-hooks to achieve the same ends.

;;; CODE

(require 'sml-proc)

;; std_in:2.1-4.3 Error: operator and operand don't agree (tycon mismatch)
;; std_in:2.1 Error: operator and operand don't agree (tycon mismatch)

(defconst sml-smlnj-error-regexp
  (concat
   "^[-= ]*\\(.+\\):"                     ;file name
   "\\([0-9]+\\)\\.\\([0-9]+\\)"          ;start line.column
   "\\(-\\([0-9]+\\)\\.\\([0-9]+\\)\\)?"  ;end line.colum
   ".+\\(\\(Error\\|Warning\\): .*\\)")   ;the message

  "Default regexp matching SML/NJ error and warning messages.

There should be no need to customise this, though you might decide
that you aren't interested in Warnings -- my advice would be to modify
`sml-error-regexp' explicitly to do that though.

If you do customise `sml-smlnj-error-regexp' you may need to modify
the function `sml-smlnj-error-parser' (qv).")

(defun sml-smlnj-error-parser (pt)
 "This parses the SML/NJ error message at PT into a 5 element list

    \(file start-line start-col end-of-err msg\)

where FILE is the file in which the error occurs\; START-LINE is the line
number in the file where the error occurs\; START-COL is the character
position on that line where the error occurs. 

If present, the fourth return value is a simple Emacs Lisp expression that
will move point to the end of the errorful text, assuming that point is at
\(start-line,start-col\) to begin with\; and MSG is the text of the error
message given by the compiler."

 ;; This function uses `sml-smlnj-error-regexp' to do the parsing, and
 ;; assumes that regexp groups 1, 2, and 3 correspond to the first three
 ;; elements of the list returned\; and groups 5, 6 and 7 correspond to the
 ;; optional elements in that order.

 (save-excursion
   (goto-char pt)
   (if (not (looking-at sml-smlnj-error-regexp))
       ;; the user loses big time.
       (list nil nil nil)
     (let ((file (match-string 1))                  ; the file
           (slin (string-to-int (match-string 2)))  ; the start line
           (scol (string-to-int (match-string 3)))  ; the start col
           (msg (if (match-beginning 7) (match-string 7))))
       ;; another loss: buggy sml/nj's produce nonsense like file:0.0 Error
       (if (zerop slin) (list file nil scol)
         ;; ok, was a range of characters mentioned?
         (if (match-beginning 4)
             ;; assume m-b 4 implies m-b 5 and m-b 6 (sml-smlnj-error-regexp)
             (let* ((elin (string-to-int (match-string 5))) ; end line
                    (ecol (string-to-int (match-string 6))) ; end col
                    (jump (if (= elin slin)
                              ;; move forward on the same line
                              `(forward-char ,(1+ (- ecol scol)))
                            ;; otherwise move down, and over to ecol
                            `(progn
                               (forward-line ,(- elin slin))
                               (forward-char ,ecol)))))
               ;; nconc glues lists together. jump & msg aren't lists
               (nconc (list file slin scol) (list jump) (list msg)))
           (nconc (list file slin scol) (list nil) (list msg))))))))

;;;###autoload
(defun sml-smlnj (pfx)
   "Set up and run Standard ML of New Jersey.
Prefix argument means accept the defaults below.

Note: defaults set here will be clobbered if you setq them in the
inferior-sml-mode-hook.

 sml-program-name  <option> \(default \"sml\"\)
 sml-default-arg   <option> \(default \"\"\) 
 sml-use-command   \"use \\\"%s\\\"\"
 sml-cd-command    \"OS.FileSys.chDir \\\"%s\\\"\"
 sml-prompt-regexp \"^[\\-=] *\"
 sml-error-regexp  sml-sml-nj-error-regexp
 sml-error-parser  'sml-sml-nj-error-parser"
   (interactive "P")
   (let ((cmd (if pfx "sml"
                (read-string "Command name: " sml-program-name)))
         (arg (if pfx ""
                (read-string "Any arguments or options (default none): "))))
     ;; sml-mode global variables
     (setq sml-program-name cmd)
     (setq sml-default-arg  arg)
     ;; buffer-local (compiler-local) variables
     (setq-default sml-use-command   "use \"%s\""
                   sml-cd-command    "OS.FieSys.chDir \"%s\""
                   sml-prompt-regexp "^[\-=] *"
                   sml-error-regexp  sml-smlnj-error-regexp
                   sml-error-parser  'sml-smlnj-error-parser)
     (sml-run cmd sml-default-arg)))

;;; Do the default setup on loading this file.

;; setqing these two may override user's hooked defaults. users
;; therefore need load this file before setting sml-program-name or
;; sml-default-arg in their inferior-sml-load-hook. sorry.

(setq         sml-program-name  "sml"
              sml-default-arg   "")

;; same sort of problem here too: users should to setq-default these
;; after this file is loaded, on inferior-sml-load-hook. as these are
;; buffer-local, users can instead set them on inferior-sml-mode-hook.

(setq-default sml-use-command   "use \"%s\""
              sml-cd-command    "OS.FileSys.chDir \"%s\""
              sml-prompt-regexp "^[\-=] *"
              sml-error-regexp  sml-smlnj-error-regexp
              sml-error-parser  'sml-smlnj-error-parser)

;;; sml-nj.el endeded
