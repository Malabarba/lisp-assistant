;;; lisp-assistant.el --- A set of functions, variables, and snippets to assist in developing lisp code.

;; Copyright (C) 2013 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/lisp-assistant
;; Version: 0.1
;; Keywords: lisp tools
;; ShortName: lisa
;; Separator: /

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
;; 0.1 - 20130813 - Created File.
;;; Code:

(defconst lisa/version "0.1" "Version of the lisp-assistant.el package.")
(defconst lisa/version-int 1 "Version of the lisp-assistant.el package, as an integer.")
(defun lisa/bug-report ()
  "Opens github issues page in a web browser. Please send me any bugs you find, and please inclue your emacs and lisa versions."
  (interactive)
  (browse-url "https://github.com/Bruce-Connor/lisp-assistant/issues/new")
  (message "Your lisa/version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           lisa/version emacs-version))
(defun lisa/customize ()
  "Open the customization menu in the `lisp-assistant' group."
  (interactive)
  (customize-group 'lisp-assistant t))

;;; ---------------------------------------------------------------------
;;; Package handling variables
(defvar lsp/package-name       nil "")
(defvar lsp/short-package-name nil "")
(defvar lsp/package-version    nil "")
(defvar lsp/package-change-log "" "")
(defcustom lsp/separator "/"
  "Separator to use after a `lsp/short-package-name'."
  :type 'string
  :group 'lisp
  :package-version '(lisp . "0.1"))
(defcustom lsp/github-username nil
  "Username used to construct github urls inside templates.

Must be exactly as it appears in your github urls. For instance, here
   https://github.com/Bruce-Connor/lisp-assistant
the username would be \"Bruce-Connor\".

Leave this nil if you don't want to use it."
  :type 'string
  :group 'lisp
  :package-version '(lisp . "0.1"))
(make-variable-buffer-local 'lsp/package-name)
(make-variable-buffer-local 'lsp/short-package-name)
(make-variable-buffer-local 'lsp/package-version)
(make-variable-buffer-local 'lsp/package-change-log)
(make-variable-buffer-local 'lsp/separator)
(defcustom lsp/default-version-number "0.1"
  "Version number for newly created packages."
  :type 'string
  :group 'lisp
  :package-version '(lisp . "0.1"))

;;; ---------------------------------------------------------------------
;;; Package handling functions
(defun lsp/insert-change-log (changeVersion &optional log)
  "Insert a commented line with date and version."
  (interactive "P")
  (let ((wasChanged (buffer-modified-p)))
    (when changeVersion (lsp/update-version-number))
    (if (called-interactively-p 'interactive)
        (setq log (read-string "Change Log: ")))
    (goto-char (point-min))
    (search-forward-regexp ";;; Change Log: *\n")
    (insert "\n")
    (forward-char -1)
    (insert ";; " lsp/package-version " - "
            (format-time-string "%Y%m%d") 
            " - ")
    (save-excursion
      (insert log "."))
    (setq lsp/package-change-log
          (concat lsp/package-change-log "\n" log "."))
    (unless wasChanged (save-buffer))))

(defun lsp/update-version-number ()
  "Change the version number of current package."
  (interactive)
  (let ((vo lsp/package-version))
    (setq lsp/package-version (read-string "New Version Number: " vo))
    (goto-char (point-min))
    (if (search-forward (concat ";; Version: " vo) nil t)
        (replace-match (concat ";; Version: " lsp/package-version))
      (setq lsp/package-version vo)
      (error "Couldn't find ';; Version:' comment. `lsp/package-version' variable not updated."))
    (if (search-forward-regexp
         (concat "(defconst *" lsp/short-package-name lsp/separator "version *\"\\(" vo "\\)")
         nil t)
        (replace-match lsp/package-version nil nil nil 1)
      (warn "Couldn't find version 'defconst'."))
    (if (search-forward-regexp
         (concat "(defconst *" lsp/short-package-name lsp/separator "version-int *\\([0-9]+\\)")
         nil t)
        (replace-match (int-to-string (1+ (string-to-number (match-string 1))))
                       nil nil nil 1))))

(defun lsp/define-package-variables ()
  "Guess package-specific variables from file name and content."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq lsp/package-name (if (search-forward-regexp "(provide '\\([^)]+\\))" nil t)
                               (match-string-no-properties 1)
                             (file-name-base (or (buffer-file-name)
                                                 (buffer-name) ""))))
    (goto-char (point-min))
    (setq lsp/short-package-name
          (if (search-forward-regexp ";;+ +ShortName: +\\([-/:a-zA-Z0-9\.]+\\)" nil t)
              (match-string-no-properties 1)
            (let ((cand (replace-regexp-in-string
                         "^\\([a-z]\\)[a-z]*\\-\\([a-z]\\)[a-z]*\\-\\([a-z]\\)[a-z]*"
                         "\\1\\2\\3" lsp/package-name)))
              (if (string= lsp/package-name cand) nil cand))))
    (goto-char (point-min))
    (setq lsp/package-version
          (if (search-forward-regexp ";;+ +Version: +\\([0-9a-zA-Z\.]+\\)" nil t)
              (match-string-no-properties 1) nil))
    (goto-char (point-min))
    (if (search-forward-regexp ";;+ +Separator: +\\([^ \n',`(){}]+\\)" nil t)
        (setq lsp/separator (match-string-no-properties 1)))))


;;; ---------------------------------------------------------------------
;;; Lisp Keymap
(define-prefix-command 'my-eval-lisp-map)
(define-key emacs-lisp-mode-map "e"      'my-eval-lisp-map)

(define-key my-eval-lisp-map "b" 'eval-buffer)
(define-key my-eval-lisp-map "d" 'eval-defun)
(define-key my-eval-lisp-map "e" 'eval-expression)
;; (define-key my-eval-lisp-map "l" 'eval-last-sexp)
(define-key my-eval-lisp-map "p" 'eval-print-last-sexp)
(define-key my-eval-lisp-map "r" 'eval-region)
(define-key my-eval-lisp-map "n"      'lsp/define-package-variables)
(define-key my-eval-lisp-map "#"      'lsp/define-package-variables)
(define-key my-eval-lisp-map "l"      'lsp/insert-change-log)
(define-key my-eval-lisp-map "f"     'lsp/find-or-define-function)
(define-key my-eval-lisp-map "v"     'lsp/find-or-define-variable)

;;;###autoload


(provide 'lisp-assistant)
;;; lisp-assistant.el ends here.
