;;; ob-reticulate.el --- Babel Functions for reticulate -*- lexical-binding: t; -*-

;; Author: Jack Kamm
;; Keywords: literate programming, reproducible research, R, Python, statistics, languages, outlines, processes
;; Package-Requires: ((org "9.4") (emacs "24.4"))
;; Homepage: https://github.com/jackkamm/ob-reticulate
;; Version: 1.0.0

;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; Org-Babel support for the R package reticulate, which enables use
;; of Python within R.

;;; Code:

(require 'ob-R)
(require 'ob-python)

(advice-add
 #'org-babel-execute:python :around #'ob-reticulate-advice)

(defun ob-reticulate-advice (orig-fun body params)
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
         (concat (format
                  "reticulate::py_run_string(\"%s\")"
                  (org-babel-python-format-session-value
                   tmp-src-file
                   (org-babel-process-file-name
                    (org-babel-temp-file "reticulate-dummy-") 'noquote)
                   nil))
		 (when (equal result-type 'value) "
reticulate::py$`__org_babel_python_final`"))
         params)))))

(provide 'ob-reticulate)

;;; ob-reticulate.el ends here
