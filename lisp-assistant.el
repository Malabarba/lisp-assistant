;;; lisp-assistant.el --- A set of functions, variables, and snippets to assist in developing lisp code.

;; Copyright (C) 2013 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/lisp-assistant
;; Version: 0.1
;; Keywords: lisp tools
;; ShortName: lisa
;; Separator: -

;;; Commentary:
;; 
;; 

;;; Instructions:
;;
;; INSTALLATION
;;
;; If you install from melpa: nothing necessary, should work out of the box.
;; If you install manually: (require 'lisp-assistant)

;;; License:
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 

;;; Change Log:
;; 0.1 - 20130814 - Imported code.
;; 0.1 - 20130813 - Created File.
;;; Code:

(defconst lisa-version "0.1" "Version of the lisp-assistant.el package.")
(defconst lisa-version-int 1 "Version of the lisp-assistant.el package, as an integer.")
(defun lisa-bug-report ()
  "Opens github issues page in a web browser. Please send me any bugs you find, and please inclue your emacs and lisa versions."
  (interactive)
  (browse-url "https://github.com/Bruce-Connor/lisp-assistant/issues/new")
  (message "Your lisa-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           lisa-version emacs-version))
(defun lisa-customize ()
  "Open the customization menu in the `lisp-assistant' group."
  (interactive)
  (customize-group 'lisp-assistant t))

;;; ---------------------------------------------------------------------
;;; Package handling variables
(defvar lisa-package-name       nil "")
(defvar lisa-short-package-name nil "")
(defvar lisa-package-version    nil "")
(defvar lisa-package-change-log "" "")
(defcustom lisa-separator "-"
  "Separator to use after a `lisa-short-package-name'.
This is usually a \"-\", but some people like to use \"/\" or
\":\"."
  :type 'string
  :group 'lisp-assistant
  :package-version '(lisp-assistant . "0.1"))
(defcustom lisa-github-username nil
  "Username used to construct github urls inside templates.

Must be exactly as it appears in your github urls. For instance, here
   https://github.com/Bruce-Connor/lisp-assistant
the username would be \"Bruce-Connor\".

Leave this nil if you don't want to use it."
  :type 'string
  :group 'lisp-assistant
  :package-version '(lisp-assistant . "0.1"))
(make-variable-buffer-local 'lisa-package-name)
(make-variable-buffer-local 'lisa-short-package-name)
(make-variable-buffer-local 'lisa-package-version)
(make-variable-buffer-local 'lisa-package-change-log)
(make-variable-buffer-local 'lisa-separator)
(defcustom lisa-default-version-number "0.1"
  "Version number for newly created packages."
  :type 'string
  :group 'lisp-assistant
  :package-version '(lisp-assistant . "0.1"))

;;; ---------------------------------------------------------------------
;;; Package handling functions
(defun lisa-insert-change-log (changeVersion &optional log)
  "Insert a commented line with date and version."
  (interactive "P")
  (let ((wasChanged (buffer-modified-p)))
    (when changeVersion (lisa-update-version-number))
    (if (called-interactively-p 'interactive)
        (setq log (read-string "Change Log: ")))
    (goto-char (point-min))
    (search-forward-regexp ";;; Change Log: *\n")
    (insert "\n")
    (forward-char -1)
    (insert ";; " lisa-package-version " - "
            (format-time-string "%Y%m%d") 
            " - ")
    (save-excursion
      (insert log "."))
    (setq lisa-package-change-log
          (concat lisa-package-change-log "\n" log "."))
    (unless wasChanged (save-buffer))))

(defun lisa-update-version-number ()
  "Change the version number of current package."
  (interactive)
  (let ((vo lisa-package-version))
    (setq lisa-package-version (read-string "New Version Number: " vo))
    (goto-char (point-min))
    (if (search-forward (concat ";; Version: " vo) nil t)
        (replace-match (concat ";; Version: " lisa-package-version))
      (setq lisa-package-version vo)
      (error "Couldn't find ';; Version:' comment. `lisa-package-version' variable not updated."))
    (if (search-forward-regexp
         (concat "(defconst *" lisa-short-package-name lisa-separator "version *\"\\(" vo "\\)")
         nil t)
        (replace-match lisa-package-version nil nil nil 1)
      (warn "Couldn't find version 'defconst'."))
    (if (search-forward-regexp
         (concat "(defconst *" lisa-short-package-name lisa-separator "version-int *\\([0-9]+\\)")
         nil t)
        (replace-match (int-to-string (1+ (string-to-number (match-string 1))))
                       nil nil nil 1))))

(defun lisa-define-package-variables ()
  "Guess package-specific variables from file name and content."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq lisa-package-name (if (search-forward-regexp "(provide '\\([^)]+\\))" nil t)
                               (match-string-no-properties 1)
                             (file-name-base (or (buffer-file-name)
                                                 (buffer-name) ""))))
    (goto-char (point-min))
    (setq lisa-short-package-name
          (if (search-forward-regexp ";;+ +ShortName: +\\([-/:a-zA-Z0-9\.]+\\)" nil t)
              (match-string-no-properties 1)
            (let ((cand (replace-regexp-in-string
                         "^\\([a-z]\\)[a-z]*\\-\\([a-z]\\)[a-z]*\\-\\([a-z]\\)[a-z]*"
                         "\\1\\2\\3" lisa-package-name)))
              (if (string= lisa-package-name cand) nil cand))))
    (goto-char (point-min))
    (setq lisa-package-version
          (if (search-forward-regexp ";;+ +Version: +\\([0-9a-zA-Z\.]+\\)" nil t)
              (match-string-no-properties 1) nil))
    (goto-char (point-min))
    (if (search-forward-regexp ";;+ +Separator: +\\([^ \n',`(){}]+\\)" nil t)
        (setq lisa-separator (match-string-no-properties 1)))))


;;; ---------------------------------------------------------------------
;;; General coding functions
(defun lisa-find-or-define-function ()
  "Like `find-function' unless function is not defined, then create a defun with it."
  (interactive)
  (if (function-called-at-point)
      (find-function (function-called-at-point))
    (push-mark)
    (let ((name (thing-at-point 'symbol)))
      (end-of-defun)
      (insert "\n(df)\n")
      (backward-char 2)
      (yas-expand)
      (insert name))))

(defun lisa-find-or-define-variable ()
  "Like `find-variable' unless variable is not defined, then create a defcustom with it."
  (interactive)
  (if (symbolp (variable-at-point))
      (find-variable (variable-at-point))
    (push-mark)
    (let ((name (thing-at-point 'symbol)))
      (beginning-of-defun)
      (insert "(dc)\n\n")
      (backward-char 3)
      (yas-expand)
      (insert name))))

(defun lisa-comment-sexp ()
  "Comment current sexp."
  (interactive)
  (if (looking-at ";")
      (progn
        (er/expand-region 1)
        (uncomment-region (region-beginning) (region-end)))
    (unless (looking-at "(")
      (condition-case nil
          (paredit-backward-up)
        (error (forward-sexp -1))))
    (mark-sexp)
    (comment-region (region-beginning) (region-end))))


;;; ---------------------------------------------------------------------
;;; Lisp-Assistant Keymap
(define-prefix-command 'lisa-map)
(define-key lisp-assistant-mode-map "e"      'lisa-map)

(define-key lisa-map "b" 'eval-buffer)
(define-key lisa-map "d" 'eval-defun)
(define-key lisa-map "e" 'eval-expression)
;; (define-key lisa-map "l" 'eval-last-sexp)
(define-key lisa-map "p" 'eval-print-last-sexp)
(define-key lisa-map "r" 'eval-region)
(define-key lisa-map "n"      'lisa-define-package-variables)
(define-key lisa-map "#"      'lisa-define-package-variables)
(define-key lisa-map "l"      'lisa-insert-change-log)
(define-key lisa-map "f"     'lisa-find-or-define-function)
(define-key lisa-map "v"     'lisa-find-or-define-variable)

;;;###autoload


(provide 'lisp-assistant)
;;; lisp-assistant.el ends here.
