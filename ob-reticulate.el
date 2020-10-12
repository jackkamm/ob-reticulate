;;; ob-reticulate.el --- Babel Functions for reticulate -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Jack Kamm
;; Keywords: literate programming, reproducible research, R, statistics
;; Homepage: https://orgmode.org

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Org-Babel support for the R package reticulate.

;;; Code:

(require 'ob-R)
(require 'ob-python)

(defalias 'org-babel-edit-prep:reticulate 'org-babel-edit-prep:R)

(advice-add
 'org-babel-execute:python :around 'org-babel-reticulate-advice)

(defun org-babel-reticulate-advice (orig-fun body params)
  (let* ((session (cdr (assq :session params)))
         (session-mode
          (and session
             (not (string= session "none"))
             (let ((session-buffer (get-buffer session)))
               (and session-buffer
                    (with-current-buffer session-buffer
                      major-mode))))))
    (if (not (eq session-mode 'inferior-ess-r-mode))
        (funcall orig-fun body params)
      (let* ((tmp-src-file (org-babel-temp-file "reticulate-"))
	     (result-type (cdr (assq :result-type params))))
        (with-temp-file tmp-src-file (insert body))
        (org-babel-execute:R
         (format (concat "reticulate::py_run_string(\"%s\")"
		         (when (equal result-type 'value) "
reticulate::py$`__org_babel_python_final`"))
                 (org-babel-python-format-session-value
                  tmp-src-file
                  (org-babel-process-file-name
                   (org-babel-temp-file "reticulate-dummy-") 'noquote)
                  nil))
         params)))))

(provide 'ob-reticulate)

;;; ob-reticulate.el ends here
