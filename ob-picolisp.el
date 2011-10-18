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
;; This library enables the use of PicoLisp in the multi-language
;; programming framework Org-Babel. PicoLisp is a minimal yet
;; fascinating lisp dialect and a highly productive application
;; framework for web-based client-server applications on top of
;; object-oriented databases. A good way to learn PicoLisp is to first
;; read Paul Grahams essay "The hundred year language" and then study
;; the various documents and essays published in the PicoLisp
;; wiki. PicoLisp is included in some GNU/Linux Distributions, and can
;; be downloaded here. It ships with a picolisp-mode and a
;; inferior-picolisp-mode for Emacs, which can be downloaded here.

;; Although it might seem more natural to use Emacs Lisp for most
;; Lisp-based programming tasks inside Org-Mode, an Emacs library
;; written in Emacs Lisp, PicoLisp has at least two outstanding
;; features that make it a valuable addition to Org-Babel:

;; PicoLisp _is_ an object-oriented database with a Prolog-based query
;; language implemented in PicoLisp (Pilog). Database objects are
;; first-class members of the language. 

;; PicoLisp is an extremely productive framework for the development
;; of interactive web-applications (on top of a database). 

;;; Requirements:

;;; Code:
;; (require 'picolisp-mode)
;; (require 'inferior-picolisp-mode)
(require 'ob)
(require 'ob-eval)

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("picolisp" . "l"))

;; optionally declare default header arguments for this language
(defvar org-babel-default-header-args:picolisp
  '((:colnames . "no"))
  "Default arguments for evaluating a picolisp source block.")

(defvar org-babel-picolisp-eoe "org-babel-picolisp-eoe"
  "String to indicate that evaluation has completed.")

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
      body)
    (if (or (member "code" result-params)
            (member "pp" result-params))
        (concat "(pretty " body ")") body)
    ))



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
	 (full-body-with-output (org-babel-expand-body:picolisp body params))
	 ;; send output to "/dev/null" if ':results value'
	 (full-body-no-output (concat (format "(out \"/dev/null\" %s"
                             full-body-with-output) ")"))
	 ;; choose body by result-type
	 (full-body (if (string= result-type "value")
			full-body-no-output
		      full-body-with-output)))


    ((lambda (result)
       (if (and (not (member 'verbatim result-params))
                (not (member 'scalar result-params))
                (> (length result) 0))
           (read result)
         result))
     (if (not (string= session-name "none"))
     ; session based evaluation
	 (org-babel-comint-with-output
	     (session (format "%S" org-babel-picolisp-eoe) nil full-body)
	   (progn 
	     (mapc
	      (lambda (line)
		(insert (org-babel-chomp line)) (comint-send-input nil t))
	      (list full-body))
	     (comint-simple-send session (format "%S" org-babel-picolisp-eoe))))
      ; external evaluation
       (let ((script-file (org-babel-temp-file "picolisp-script-")))
	 (with-temp-file script-file
	   (insert (concat full-body "(bye)"))
	   (org-babel-eval
	    (format "%s %s"
		    org-babel-picolisp-cmd
		    (org-babel-process-file-name script-file))
	    "")))))))

(defun org-babel-picolisp-initiate-session (&optional session-name)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (unless (string= session-name "none")
    (let ((session-buffer (save-window-excursion
                            (run-picolisp org-babel-picolisp-cmd
			     ; picolisp-program-name
			     )
                            (rename-buffer session-name)
                            (current-buffer))))
      (if (org-babel-comint-buffer-livep session-buffer)
          (progn (sit-for .25) session-buffer)
        (sit-for .5)
        (org-babel-picolisp-initiate-session session-name)))))




(provide 'ob-picolisp)
;;; ob-picolisp.el ends here
