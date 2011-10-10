;;; ob-picolisp.el --- org-babel functions for picolisp evaluation

;; Copyright (C) Thorsten

;; Author: Thorsten and Eric Schulte
;; Keywords: literate programming, reproducible research
;; Homepage: http://orgmode.org
;; Version: 0.01

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file is not intended to ever be loaded by org-babel, rather it
;; is a template for use in adding new language support to Org-babel.
;; Good first steps are to copy this file to a file named by the
;; language you are adding, and then use `query-replace' to replace
;; all strings of "template" in this file with the name of your new
;; language.
;;
;; If you have questions as to any of the portions of the file defined
;; below please look to existing language support for guidance.
;;
;; If you are planning on adding a language to org-babel we would ask
;; that if possible you fill out the FSF copyright assignment form
;; available at http://orgmode.org/request-assign-future.txt as this
;; will make it possible to include your language support in the core
;; of Org-mode, otherwise unassigned language support files can still
;; be included in the contrib/ directory of the Org-mode repository.

;;; Requirements:

;; Use this section to list the requirements of this language.  Most
;; languages will require that at least the language be installed on
;; the user's system, and the Emacs major mode relevant to the
;; language be installed as well.

;;; Code:
(require 'ob)
(require 'ob-eval)

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("picolisp" . "l"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:picolisp
  '((:colnames . "no"))
  "Default arguments for evaluating a picolisp source block.")


(defcustom org-babel-picolisp-cmd "pil"
  "Name of command used to evaluate picolisp blocks."
  :group 'org-babel
  :type 'string)

(defun org-babel-expand-body:picolisp (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (mapcar #'cdr (org-babel-get-header params :var)))
        (result-params (cdr (assoc :result-params params)))
        (print-level nil) (print-length nil))
    (if (> (length vars) 0)
        (concat "(prog (let ("
                (mapconcat
                 (lambda (var)
                   (format "%S '%S)"
                           (print (car var))
                           (print (cdr var))))
                 vars "\n      ")
                " \n" body ") )")
      body)))

(defun org-babel-execute:picolisp (body params)
  "Execute a block of Picolisp code with org-babel.  This function is
 called by `org-babel-execute-src-block'"           
  (message "executing Picolisp source code block")
  (let* (
	;; name of the session or "none"
        (session-name (cdr (assoc :session params)))
        ;; set the session if the session variable is non-nil
        (session (org-babel-picolisp-initiate-session session-name))
        ;; either OUTPUT or VALUE which should behave as described above
	(result-type (cdr (assoc :result-type params)))
        ;; expand the body with `org-babel-expand-body:picolisp'
        (full-body (org-babel-expand-body:picolisp body params))
        (script-file (org-babel-temp-file "picolisp-script-")))

    ;; (read
    ;;  (if (not (string= session-name "none"))
         ;; session evaluation
         ;; (org-babel-comint-with-output
         ;;       (session (format "%S" org-babel-picolisp-eoe) t full-body)
         ;;     (mapc
         ;;      (lambda (line)
         ;;        (insert (org-babel-chomp line)) (comint-send-input nil t))
         ;;      (list full-body (format "%S" org-babel-picolisp-eoe))))

    (with-temp-file script-file
      (insert (concat (if (string= result-type "value")
                          ;(format "(print %s)" full-body)
			  (concat (format "(print (out \"/dev/null\" %s)"
					  full-body) ")")
                        full-body)
                      "(bye)")))
    ((lambda (result)
       (if (and (not (member 'verbatim result-params))
                (not (member 'scalar result-params))
                (> (length result) 0))
           (read result)
         result))
     (org-babel-eval
      (format "%s %s"
              org-babel-picolisp-cmd
              (org-babel-process-file-name script-file))
      ""))))

(defun org-babel-picolisp-initiate-session (&optional session)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (unless (string= session "none")
    (let ((session-buffer (save-window-excursion
                            (run-picolisp picolisp-program-name)
                            (rename-buffer session)
                            (current-buffer))))
      (if (org-babel-comint-buffer-livep session-buffer)
          (progn (sit-for .25) session-buffer)
        (sit-for .5)
        (org-babel-picolisp-initiate-session session)))))


(provide 'ob-picolisp)
;;; ob-picolisp.el ends here
