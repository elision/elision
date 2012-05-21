;;; elision-mode-el -- Major mode for editing Elision input files

;; Author: Stephen Lindberg <sllindberg21@students.tntech.edu>
;; Created: 15 May 2012
;; Keywords: Elision major-mode

;; Copyright (C) 2012 UT-Battelle, LLC.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;;; Code: 
(defvar elision-mode-hook nil)
(defvar elision-mode-map
  (let ((elision-mode-map (make-keymap)))
    (define-key elision-mode-map "\C-j" 'newline-and-indent)
	(define-key elision-mode-map [remap comment-dwim] 'elision-comment-dwim)
    elision-mode-map)
  "Keymap for elision major mode")
  
  
  (add-to-list 'auto-mode-alist '("\\.elision\\'" . elision-mode))
  (add-to-list 'auto-mode-alist '("\\.eli\\'" . elision-mode))

	(set-face-foreground 'font-lock-doc-face "Black")
	(set-variable font-lock-doc-face 'font-lock-doc-face)
 
;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq elision-font-lock-keywords
	(list
		;'("\"\"\"\\(\n\\|.\\)*?\"\"\"" . font-lock-comment-face) ; verbatim blocks
		'("[\\]\\$\\$?`\\([\\]`\\|[^`]\\)*`" . font-lock-function-name-face) ; back-ticked lambda
		'("[\\]\\$\\$?[_a-zA-Z][_a-zA-Z0-9]*" . font-lock-function-name-face) ; lambda
		'("\\$\\$?`\\([\\]`\\|[^`]\\)*`" . font-lock-variable-name-face) ; back-ticked variable
		'("\\$\\$?[_a-zA-Z][_a-zA-Z0-9]*" . font-lock-variable-name-face) ; variable
		'("##?`\\([\\]`\\|[^`]\\)*`" . font-lock-variable-name-face) ; sf-bound back-ticked term
		'("#\\(#\\)?[_a-zA-Z][_a-zA-Z0-9]*" . font-lock-variable-name-face) ; sf-bound term

		'("true\\|false\\|Nothing" . font-lock-constant-face) ; boolean constants
		'("operator\\|is\\|case\\|->\\|@\\|=" . font-lock-keyword-face) ; keywords
		'("associative\\|commutative\\|idempotent\\|identity\\|absorber\\|not" . font-lock-builtin-face) ; algebraic properties

		'("%\\([!]?[ACIBD]\\(\\[.+?\\]\\)?\\)*" . font-lock-variable-name-face) ; lists
		
		'(": *`\\([\\]`\\|[^`]\\)*`" . font-lock-type-face) ; back-ticked symbol types
		'(": *[_a-zA-Z][_a-zA-Z0-9]*" . font-lock-type-face) ; symbol types
		'("ANY\\|BINDING\\|BOOLEAN\\|FLOAT\\|INTEGER\\|OPTYPE\\|STRATEGY\\|STRING\\|SYMBOL\\|NONE\\|\\^TYPE" . font-lock-type-face) ; root types

		'("[_a-zA-Z][_a-zA-Z0-9]*[(.]" . font-lock-function-name-face) ; operator application
		'("[{]:\\|:[}]" . font-lock-builtin-face) ; special forms
		'("(\\|)\\|\\[\\|\\]" . font-lock-builtin-face) ; code block operators

		'("-?0[xX][0-9a-fA-F]*\\([0-9a-fA-F]+[.]\\)?[0-9a-fA-F]+p?" . font-lock-constant-face) ; hex numerical constants 
		'("-?0[bB][0-1]*\\([0-1]+[.]\\)?[0-1]+[ep]?" . font-lock-constant-face) ; binary numerical constants 
		'("-?[0-9]*\\([0-9]+[.]\\)?[0-9]+[ep]?" . font-lock-constant-face) ; decimal numerical constants


		
	)
)


;; the command to comment/uncomment text
(defun elision-comment-dwim (arg)
"Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
   (interactive "*P")
   (require 'newcomment)
   (let ((deactivate-mark nil) (comment-start "\"\"\"") (comment-end "\"\"\""))
     (comment-dwim arg))
)



;; define our auto-indent rules
(defun elision-indent-line ()
  "Indent current line as Elision code."
  (interactive)
  (beginning-of-line)
  (if (bobp)
	  (indent-line-to 0)		   ; First line is always non-indented
	(let ((not-indented t) cur-indent)
	  (if (looking-at "^[ \t]*\\}") ; If the line we are looking at is the end of a block, then decrease the indentation
		  (progn
			(save-excursion
			  (forward-line -1)
			  (setq cur-indent (- (current-indentation) default-tab-width)))
			(if (< cur-indent 0) ; We can't indent past the left margin
				(setq cur-indent 0)))
		(save-excursion
		  (while not-indented ; Iterate backwards until we find an indentation hint
			(forward-line -1)
			(if (looking-at "^[ \t]*\\]") ; This hint indicates that we need to indent at the level of the END_ token
				(progn
				  (setq cur-indent (current-indentation))
				  (setq not-indented nil))
			  (if (looking-at "^[ \t]*\\[") ; This hint indicates that we need to indent an extra level
				  (progn
					(setq cur-indent (+ (current-indentation) default-tab-width)) ; Do the actual indenting
					(setq not-indented nil))
				(if (bobp)
					(setq not-indented nil)))))))
	  (if cur-indent
		  (indent-line-to cur-indent)
		(indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation




;; create our syntax table
(defvar elision-mode-syntax-table
  (let ((elision-mode-syntax-table (make-syntax-table)))
	
    ; This is added so entity names with underscores can be more easily parsed
	(modify-syntax-entry ?_ "w" elision-mode-syntax-table)
	
	; C++ style comments
	(modify-syntax-entry ?/ ". 124b" elision-mode-syntax-table)
	(modify-syntax-entry ?* ". 23" elision-mode-syntax-table)
	(modify-syntax-entry ?\n "> b" elision-mode-syntax-table)
	;(modify-syntax-entry ?\" "w" elision-mode-syntax-table) ;"
	elision-mode-syntax-table)
  "Syntax table for elision-mode")

  
 ;; define our mode 
(defun elision-mode ()
  (interactive)
  (kill-all-local-variables)
  (use-local-map elision-mode-map)
  (set-syntax-table elision-mode-syntax-table)
  ;; Set up font-lock
  (set (make-local-variable 'font-lock-defaults) '(elision-font-lock-keywords))
  ;; Register our indentation function
  (set (make-local-variable 'indent-line-function) 'elision-indent-line)  
  (setq major-mode 'elision-mode)
  (setq mode-name "Elision")
  (run-hooks 'elision-mode-hook)
  (setq font-lock-extend-region-multiline t)
	;; auto line-wrapping
	(setq longlines-auto-wrap t)
	(setq longlines-show-hard-newlines t)
	(setq longlines-wrap-follows-window-size t)
	;; verbatim blocks
	(require 'newcomment)
	(setq comment-multi-line t)
	(setq comment-start "\"\"\"")
	(setq comment-end "\"\"\"")
	(setq comment-style extra-line)
)



;; lastly, provide the mode
(provide 'elision-mode)
  
  