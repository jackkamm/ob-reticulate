;;; ob-reticulate.el --- Babel Functions for reticulate -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Free Software Foundation, Inc.

;; Author: Jack Kamm
;; Keywords: literate programming, reproducible research, R, statistics, languages, outlines, processes
;; Package-Requires: ((org "9.4") (emacs "24.4"))
;; Homepage: https://github.com/jackkamm/ob-reticulate
;; Version: 1.0.0

;;; Commentary:

;; Org-Babel support for the R package reticulate.

;;; Code:

(require 'ob-R)
(require 'ob-python)

(declare-function org-babel-python-format-session-value "ext:ob-python"
                  (src-file result-file result-params))

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
