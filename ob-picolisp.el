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

(defun org-babel-run-picolisp (cmd)
  "Run an inferior Picolisp process, input and output via buffer `*picolisp*'.
If there is a process already running in `*picolisp*', switch to that buffer.
With argument, allows you to edit the command line (default is value
of `picolisp-program-name').
Runs the hook `inferior-picolisp-mode-hook' \(after the `comint-mode-hook'
is run).
\(Type \\[describe-mode] in the process buffer for a list of commands.)"

  (when (not (comint-check-proc "*picolisp*"))
    (let ((cmdlist (split-string cmd)))
      (set-buffer (apply 'make-comint "picolisp" (car cmdlist)
                         nil (cdr cmdlist) ) )
      (inferior-picolisp-mode) ) )
  (setq picolisp-program-name cmd)
  (setq picolisp-buffer "*picolisp*") )



  ;; (when (not (comint-check-proc session-name))
  ;;   (let ((cmdlist (split-string cmd)))
  ;;     (set-buffer (apply 'make-comint session-name (car cmdlist)
  ;;                        nil (cdr cmdlist) ) )
  ;;     (inferior-picolisp-mode) ) )
  ;; (setq picolisp-program-name cmd)
  ;; (setq picolisp-buffer (format "*%S*" session-name) ))


;; wouldn't loading the temp-file made for external execution into the inferior Picolisp session all thats necesary?

;; (defun org-babel-picolisp-load-file (file-name)
;;   "Load (non-interactively) a Picolisp file FILE-NAME into the inferior Picolisp process."
;;   (comint-check-source file-name) ; Check to see if buffer needs saved.
;;   (comint-send-string session (concat "(load \""
;;                                               file-name
;;                                               "\"\)\n" ) ) )


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


;; (defun org-babel-execute:picolisp (body params)
;;   "Execute a block of Picolisp code with org-babel.  This function is
;;  called by `org-babel-execute-src-block'"           
;;   (message "executing Picolisp source code block")
;;   (let ((result-type (cdr (assoc :result-type params)))
;;         (full-body (org-babel-expand-body:picolisp body params))
;;         (script-file (org-babel-temp-file "picolisp-script-")))
;;     (with-temp-file script-file
;;       (insert (concat (if (string= result-type "value")
;;                           (format "(print %s)" full-body)
;;                         full-body)
;;                       "(bye)")))
;;     ((lambda (result)
;;        (if (and (not (member 'verbatim result-params))
;;                 (not (member 'scalar result-params))
;;                 (> (length result) 0))
;;            (read result)
;;          result))
;;      (org-babel-eval
;;       (format "%s %s"
;;               org-babel-picolisp-cmd
;;               (org-babel-process-file-name script-file))
;;       ""))))


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
        (full-body (org-babel-expand-body:picolisp body params)))

    ;; (read
     ;; (if (not (string= session-name "none"))
     ;;     session evaluation
     ;;     (org-babel-comint-with-output
     ;;           (session (format "%S" org-babel-picolisp-eoe) t full-body)
     ;;         (mapc
     ;;          (lambda (line)
     ;;            (insert (org-babel-chomp line)) (comint-send-input nil t))
     ;;          (list full-body (format "%S" org-babel-picolisp-eoe))))



      ;; (insert (if (string= result-type "value")
      ;;             ;(format "(print %s)" full-body)
      ;; 		  (concat (format "(print (out \"/dev/null\" %s)"
      ;; 				  full-body) ")")
      ;; 		full-body))

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

	 
	   ;; (org-babel-comint-with-output
	   ;;     (session (format "%S" org-babel-picolisp-eoe) t full-body)
	   ;;   (progn
	   ;;     (org-babel-picolisp-load-file script-file)	       
	   ;;     (comint-simple-send session (format "%S" org-babel-picolisp-eoe))))
       ; external evaluation
       ;; (goto-char point-max)
       ;; (insert "\n(bye)")
       (let ((script-file (org-babel-temp-file "picolisp-script-")))
       (with-temp-file script-file
         (insert (concat (if (string= result-type "value")
                             (concat (format "(out \"/dev/null\" %s"
                                             full-body) ")")
                           full-body)
                         "(bye)"))

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
