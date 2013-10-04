;;; cypher-mode.el --- major mode for editing cypher scripts

;; Copyright 2013 François-Xavier Bois

;; Version: 0.0.1
;; Author: François-Xavier Bois <fxbois AT Google Mail Service>
;; Maintainer: François-Xavier Bois
;; Created: Sept 2013
;; Keywords: cypher graph
;; URL: http://github.com/fxbois/cypher-mode
;; Repository: http://github.com/fxbois/cypher-mode

;; =========================================================================
;; This work is sponsored by Kernix : Digital Agency (Web & Mobile) in Paris
;; =========================================================================

;; This file is not part of Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;; Code goes here

(defgroup cypher nil
  "Major mode for editing cypher scripts."
  :version "0.0.1"
  :group 'languages)

(defgroup cypher-faces nil
  "Faces for syntax highlighting."
  :group 'cypher-mode
  :group 'faces)

(defface cypher-clause-face
  '((t :inherit font-lock-builtin-face))
  "Face for language clauses."
  :group 'cypher-faces)

(defface cypher-keyword-face
  '((t :inherit font-lock-keyword-face))
  "Face for language keywords."
  :group 'cypher-faces)

(defface cypher-function-face
  '((t :inherit font-lock-function-name-face))
  "Face for language function."
  :group 'cypher-faces)

(defface cypher-node-face
  '((t :inherit font-lock-constant-face))
  "Face for language keywords."
  :group 'cypher-faces)

(defface cypher-relation-face
  '((t :inherit font-lock-type-face))
  "Face for language keywords."
  :group 'cypher-faces)

(defface cypher-symbol-face
  '((t :inherit font-lock-variable-name-face))
  "Face for language keywords."
  :group 'cypher-faces)

(defvar cypher-clauses
  (regexp-opt
   '("create" "create unique" "delete" "desc" "foreach" "limit" "match"
     "order by" "return" "set" "start" "union" "where" "with"))
  "Cypher clauses.")

(defvar cypher-keywords
  (regexp-opt
   '("in" "as"
     ;; "abs" "all" "and" "any" "as" "avg"
     ;; "coalesce" "collect" "count" "cypher"
     ;; "desc" "distinct" "extract" "filter"
     ;; "has" "head" "id" "in" "is"
     ;; "last" "left" "length" "limit" "lower" "ltrim"
     ;; "node" "node_auto_index" "nodes" "none" "not" "null"
     ;; "or" "order by"
     ;; "range" "reduce" "reduce" "relationships" "replace" "right"
     ;; "round" "rtrim"
     ;; "sign" "single" "sqrt" "str" "substring"
     ;; "tail" "trim" "type" "upper"
     ))
  "Cypher keywords.")

(defvar cypher-functions
  (regexp-opt
   '(""))
  "Cypher functions")

(defvar cypher-font-lock-keywords
  (list
   (cons (concat "\\<\\(" cypher-clauses "\\)\\>") '(1 'cypher-clause-face))
   (cons (concat "\\<\\(" cypher-keywords "\\)\\>") '(1 'cypher-keyword-face))
   '("\\([[:alpha:]_:]+\\)[ ]?(" 1 'cypher-function-face)
   '("-\\[\\(?:[[:alnum:]_]+\\)?\\(:[[:alnum:]_]+\\)" 1 'cypher-relation-face)
   '("(\\(?:[[:alnum:]_]+\\)?\\(:[[:alnum:]_]+\\)[ ]?[{)]" 1 'cypher-node-face)
   '("\\([[:alnum:]_]+[ ]?:\\)" 1 'cypher-symbol-face)
   ))

 (defvar cypher-mode-syntax-table
   (let ((table (make-syntax-table)))
     ;; _   : word
     (modify-syntax-entry ?_ "w" table)
     ;; //  : comment
     (modify-syntax-entry ?\/ ". 12b" table)
     (modify-syntax-entry ?\n "> b" table)
     ;; ' " : strings
     (modify-syntax-entry ?\" "\"" table)
     (modify-syntax-entry ?\' "\"" table)
     (modify-syntax-entry ?\` "\"" table)
     table)
   "Syntax table.")

(defvar cypher-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c r") 'cypher-reload)
    ))

(eval-and-compile
  (defalias 'cypher-prog-mode
    (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))
  (if (fboundp 'with-silent-modifications)
      (defalias 'cypher-with-silent-modifications 'with-silent-modifications)
    (defmacro cypher-with-silent-modifications (&rest body)
      "For compatibility with Emacs pre 23.3"
      `(let ((old-modified-p (buffer-modified-p))
             (inhibit-modification-hooks t)
             (buffer-undo-list t))
         (unwind-protect
             ,@body
           (set-buffer-modified-p old-modified-p)))))
  )

(defvar cypher-font-lock-defaults
  '(cypher-font-lock-keywords
    nil
    t
;;    (("_" . "w"))
;;    nil
    ))

;;;###autoload
(define-derived-mode cypher-mode cypher-prog-mode "Cypher"
  "Major mode for editing web templates."
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start)
  (make-local-variable 'font-lock-defaults)
;;  (make-local-variable 'font-lock-keywords-case-fold-search)
  (make-local-variable 'indent-line-function)
  (setq comment-end ""
        comment-start "//"
        font-lock-defaults cypher-font-lock-defaults
        indent-line-function 'indent-relative)
  )

(defun cypher-reload ()
  "Reload cypher."
  (interactive)
  (cypher-with-silent-modifications
   (unload-feature 'cypher-mode)
   (cypher-mode)
   (if (fboundp 'cypher-mode-hook)
       (cypher-mode-hook))))

(defun cypher-indent-line ()
  "Indent current line."
  (save-excursion
    (let ((inhibit-modification-hooks t))
      0)))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cypher\\'" . cypher-mode))

(provide 'cypher-mode)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; cypher-mode.el ends here
