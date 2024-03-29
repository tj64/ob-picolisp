;;; ob-picolisp.el --- Babel Functions for Picolisp  -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2018 Free Software Foundation, Inc.

;; Authors: Thorsten Jolitz
;;       Eric Schulte
;; Keywords: literate programming, reproducible research
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

;; This library enables the use of PicoLisp in the multi-language
;; programming framework Org-Babel. PicoLisp is a minimal yet
;; fascinating lisp dialect and a highly productive application
;; framework for web-based client-server applications on top of
;; object-oriented databases. A good way to learn PicoLisp is to first
;; read Paul Grahams essay "The hundred year language"
;; (http://www.paulgraham.com/hundred.html) and then study the various
;; documents and essays published in the PicoLisp wiki
;; (https://picolisp.com/wiki/?home). There is also a PicoLisp Blog out
;; there (https://picolisp-explored.com/), as well as a huge amount of
;; code examples on https://rosettacode.org/wiki/Category:PicoLisp,
;; written by the creator of PicoLisp himself.
;;
;; PicoLisp is included in some GNU/Linux Distributions, and can be
;; downloaded here: http://software-lab.de/down.html.
;; It used to ship with a picolisp-mode and an inferior-picolisp-mode
;; for Emacs (to be found in the /lib/el/ directory) until Pil64, but
;; from Pil21 on these libraries have to be found on GitHub
;; (e.g. https://github.com/tj64). The same holds for former library
;; files for line editing (led.l and eled.l, replaced by readline lib
;; in Pil21) and for symbol editing (edit.l and eedit.l, replaced by
;; the use of VIP, the editor written in and shipped with Pil21).
;;
;; There are two Emacs modes now, picolisp-mode (older, more official)
;; and plist-mode (newer, less tested). Both rely on the same
;; inferior-picolisp.el file.

;; Although it might seem more natural to use Emacs Lisp for most
;; Lisp-based programming tasks inside Org, an Emacs library written
;; in Emacs Lisp, PicoLisp has at least two outstanding features that
;; make it a valuable addition to Org Babel:

;; PicoLisp _is_ an object-oriented database with a Prolog-based query
;; language implemented in PicoLisp (Pilog). Database objects are
;; first-class members of the language.

;; PicoLisp is an extremely productive framework for the development
;; of interactive web-applications (on top of a database).

;;; Requirements:

;;; Code:
(require 'ob)
(require 'comint)

(declare-function run-picolisp "ext:inferior-picolisp" (cmd))
(defvar org-babel-tangle-lang-exts) ;; Autoloaded

;; optionally define a file extension for this language
(add-to-list 'org-babel-tangle-lang-exts '("picolisp" . "l"))

;;; interferes with settings in org-babel buffer?
;; optionally declare default header arguments for this language
;; (defvar org-babel-default-header-args:picolisp
;;   '((:colnames . "no"))
;;   "Default arguments for evaluating a picolisp source block.")

(defvar org-babel-picolisp-eoe "org-babel-picolisp-eoe"
  "String to indicate that evaluation has completed.")

(defvar org-babel-picolisp-use-global-install-p nil
  "Non-nil, if the global PicoLisp installation should be used.

Otherwise, try to find and call a local installation before falling back to
the global installation.")

;;; Customs

(defcustom org-babel-picolisp-cmd "pil"
  "Name of command used to evaluate picolisp blocks."
  :group 'org-babel
  :version "28.2"
  :type 'string)

;;; Functions
;;; Non-Interactive Functions

;; courtesy of Michael Heerdegen
(defun org-babel-picolisp--split-path (path)
  (org-babel-picolisp--split-path-1 path ()))

;; courtesy of Michael Heerdegen
(defun org-babel-picolisp--split-path-1 (path accum)
  (let ((dir  (directory-file-name (file-name-directory path)))
        (name (file-name-nondirectory path)))
    (if (equal dir path)
        accum
      (org-babel-picolisp--split-path-1 dir (cons name accum)))))

(defun org-babel-picolisp-local-cmd ()
  "Return a context-sensitive call to a local PicoLisp installation.

Try to figure out if the Org-mode file with PicoLisp
source-blocks to be executed is located inside a (local) PicoLisp
installation, and if so, at what relative level in the file
hierachy. Return a call to the containing local PicoLisp
installation or fall back to `org-babel-picolisp-cmd'."
  (let* ((file-name-as-list
          (when buffer-file-name
            (org-babel-picolisp--split-path
             (expand-file-name buffer-file-name))))
         (path-length
          (1- (length (member "picoLisp" file-name-as-list)))))
    (if (and (integer-or-marker-p path-length)
             (< path-length 0))
        org-babel-picolisp-cmd
      (cl-case path-length
        (1 "./pil")
        (2 "../pil")
        (t (let ((cmd "pil"))
             (dotimes (i (1- path-length) cmd)
                   (setq cmd (concat "../" cmd)))))))))


(defun org-babel-expand-body:picolisp (body params &optional processed-params)
  "Expand BODY according to PARAMS, return the expanded body."
  (let ((vars (org-babel--get-vars params))
        (print-level nil)
        (print-length nil))
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
         ;; Name of the session or "none".
         (session-name (cdr (assq :session params)))
         ;; Set the session if the session variable is non-nil.
         (session (org-babel-picolisp-initiate-session session-name))
         ;; Either OUTPUT or VALUE which should behave as described above.
         (result-params (cdr (assq :result-params params)))
         ;; Expand the body with `org-babel-expand-body:picolisp'.
         (full-body (org-babel-expand-body:picolisp body params))
         ;; Wrap body appropriately for the type of evaluation and results.
         (wrapped-body
          (cond
           ((or (member "code" result-params)
                (member "pp" result-params))
            (format "(pretty (out \"/dev/null\" %s))" full-body))
           ((and (member "value" result-params) (not session))
            (format "(print (out \"/dev/null\" %s))" full-body))
           ((member "value" result-params)
            (format "(out \"/dev/null\" %s)" full-body))
           (t full-body))))

    ((lambda (result)
       (if (or (member "verbatim" result-params)
               (member "scalar" result-params)
               (member "output" result-params)
               (member "code" result-params)
               (member "pp" result-params)
               (= (length result) 0))
           result
         (read result)))
     (if (not (string= session-name "none"))
         ;; session based evaluation
         (mapconcat ;; <- joins the list back together into a single string
          #'identity
          (butlast ;; <- remove the org-babel-picolisp-eoe line
           (delq nil
                 (mapcar
                  (lambda (line)
                    (org-babel-chomp ;; remove trailing newlines
                     (when (> (length line) 0) ;; remove empty lines
                       (cond
                        ;; remove leading "-> " from return values
                        ((and (>= (length line) 3)
                              (string= "-> " (subseq line 0 3)))
                         (subseq line 3))
                        ;; remove trailing "-> <<return-value>>" on the
                        ;; last line of output
                        ((and (member "output" result-params)
                              (string-match-p "->" line))
                         (subseq line 0 (string-match "->" line)))
                        (t line)
                        )
                       ;; (if (and (>= (length line) 3) ;; remove leading "<- "
                       ;;          (string= "-> " (subseq line 0 3)))
                       ;;     (subseq line 3)
                       ;;   line)
                       )))
                  ;; returns a list of the output of each evaluated expression
                  (org-babel-comint-with-output (session org-babel-picolisp-eoe)
                    (insert wrapped-body) (comint-send-input)
                    (insert "'" org-babel-picolisp-eoe) (comint-send-input)))))
          "\n")
       ;; external evaluation
       (let ((script-file (org-babel-temp-file "picolisp-script-")))
         (with-temp-file script-file
           (insert (concat wrapped-body "(bye)")))
         (org-babel-eval
          (format "%s %s"
                  (if org-babel-picolisp-use-global-install-p
                      org-babel-picolisp-cmd
                    (org-babel-picolisp-local-cmd))
                  (org-babel-process-file-name script-file))
          ""))))))

(defun org-babel-picolisp-initiate-session (&optional session-name)
  "If there is not a current inferior-process-buffer in SESSION
then create.  Return the initialized session."
  (unless (string= session-name "none")
    (require 'inferior-picolisp)
    ;; provide a reasonable default session name
    (let ((session (or session-name "*inferior-picolisp*")))
      ;; check if we already have a live session by this name
      (if (org-babel-comint-buffer-livep session)
          (get-buffer session)
        (save-window-excursion
          (run-picolisp
           (if (not org-babel-picolisp-use-global-install-p)
                      org-babel-picolisp-cmd
                    (org-babel-picolisp-local-cmd)))
          (rename-buffer session-name)
          (current-buffer))))))

;;; Commands

(defun org-babel-picolisp-toggle-cmd (&optional arg)
  "Toggle between use of global or local PicoLisp installation.

When `org-babel-picolisp-use-global-install-p' is nil, it will be
set to t, otherwise it will be set to nil. With prefix argument
ARG, use the global PicoLisp installation if ARG is positive,
otherwise try calling a local installation before falling back to
the global installation."
  (interactive "P")
  (setq org-babel-picolisp-use-global-install-p
        (if (null arg)
            (not org-babel-picolisp-use-global-install-p)
          (> (prefix-numeric-value arg) 0)))
  (message "Use global installation of PicoLisp set to %s"
           org-babel-picolisp-use-global-install-p))

;;; Provide

(provide 'ob-picolisp)
;; ob-picolisp.el ends here
