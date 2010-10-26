;;; ----------------------------------------------------------------------
;;; Copyright 2010 Alexey Radul.
;;; ----------------------------------------------------------------------
;;; This file is part of Propagator Network Prototype.
;;; 
;;; Propagator Network Prototype is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;; 
;;; Propagator Network Prototype is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;; 
;;; You should have received a copy of the GNU General Public License
;;; along with Propagator Network Prototype.  If not, see <http://www.gnu.org/licenses/>.
;;; ----------------------------------------------------------------------

;;; The purpose of these Emacs modifications is to let me write
;;; define-foo and let-foo macros, and have Emacs treat them like the
;;; standard define and let.

;;; This works by tweaking some of the guts of Emacs' Scheme Mode (via
;;; scheme.el).  There are two things that need to be done:
;;; indentation and colorization (for font-lock mode).  Indentation is
;;; easy but repetitive.  Emacs Scheme Mode already does the thing I
;;; want for define-mumble.  For the let-style macros I have, I write

(put 'let-cells 'scheme-indent-function 1)
(put 'let-cells* 'scheme-indent-function 1)
(put 'let-cells-rec 'scheme-indent-function 1)
(put 'let-cell 'scheme-indent-function 1)
(put 'let-cell-rec 'scheme-indent-function 1)

;;; Colorizing is more involved but also more general:

;; Modified from scheme-font-lock-keywords-1 from scheme.el
(defconst axchify-scheme-font-lock-keywords
  (eval-when-compile
    (list
     ;;
     ;; Declarations.  Hannes Haug <hannes.haug@student.uni-tuebingen.de> says
     ;; this works for SOS, STklos, SCOOPS, Meroon and Tiny CLOS.
     (list (concat "(\\(define\\*?\\("
                   ;; Class names.
                   "-class"
                   ;; Guile modules.
                   "\\|-module\\|"
                   ;; Macro names, as variable names.  A bit dubious, this.
                   "\\(-syntax\\|-macro\\)\\|"
                   ;; Function names, and names of arguments of user-defined
                   ;; definition mechanisms
                   "\\(\\|-public\\|-method\\|-generic\\(-procedure\\)?\\|-\\sw+\\)\\|"
                   "\\)\\)\\>"
                   ;; Any whitespace and declared object.
                   "[ \t]*(?"
                   "\\(\\sw+\\)?")
           '(1 font-lock-keyword-face)
           '(6 (cond ((match-beginning 4) font-lock-function-name-face)
                     ((match-beginning 3) font-lock-variable-name-face)
                     (t font-lock-type-face))
               nil t))
     ;; General let-like forms
     (cons "(\\(let-\\sw+\\)" 1) 
     ))
  "General definitions to highlight in Scheme modes.")

;; Colorize and indent let-foo and define-foo macros consistently
;; with let and define
(font-lock-add-keywords 'scheme-mode axchify-scheme-font-lock-keywords)
