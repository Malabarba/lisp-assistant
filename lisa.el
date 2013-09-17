;;; lisa.el --- Lisp Assistant. A set of functions, variables, and snippets to assist in developing lisp code.

;; Copyright (C) 2013 Artur Malabarba <bruce.connor.am@gmail.com>

;; Author: Artur Malabarba <bruce.connor.am@gmail.com>
;; URL: http://github.com/Bruce-Connor/lisp-assistant
;; Version: 0.5
;; Package-Requires: ((yasnippet "0.8.0"))
;; Keywords: lisp tools
;; Prefix: lisa
;; Separator: -

;;; Commentary:
;; 
;; Meet Lisa, your Lisp Assistant.
;;
;; `lisa-mode' is a minor-mode which defines a series of features to
;; aid in lisp development. It defines:
;;   1. a few useful functions,
;;   2. keybindings for these functions,
;;   3. buffer-local variables which contain package data (package name, version number, etc),
;;   4. yasnippet templates to aid in developing lisp code.
;; 
;; Most useful functions are: 
;;   1. `lisa-insert-change-log'
;;   2. `lisa-update-version-number'
;;   3. `lisa-find-or-define-function'
;;   4. `lisa-find-or-define-variable'
;; (see the `lisa-mode' function description for the others)
;; 
;; Most useful yasnippets are:
;; 
;;     (df ---> Expands to a full defun
;;     (dc ---> Expands to a full defcustom
;; (see the `lisa-mode' function description for the others)
;; 
;; These two snippets make use of some package variables (see the doc
;; of `lisa-define-package-variables' for more information). These
;; variables make the snippets super useful, and they are
;; automatically defined whenever you open an existing .el file or
;; create one from a template. If you create your file from scratch,
;; these won't be automatically defined, but you can use
;; `lisa-define-package-variables' to redefine them.

;;; Instructions:
;;
;; INSTALLATION
;;
;; The most basic installation process it to contact Lisa and then hire her:
;; 
;;     (require 'lisa)
;;     (add-hook 'emacs-lisp-mode-hook 'lisa-mode)
;;
;; This will define several useful functions, the lisa-mode-map, and
;; the snippets.
;;
;; The best way to learn about all of these is to read the description
;; of lisa-mode (C-h f lisa-mode RET).
;;
;; To get the most out of Lisa, you need to give her a bit of data to
;; work with, so here's a sample configuration:
;;
;;     (require 'lisa)
;;     (setq lisa-helpful-keymap t)
;;     (setq lisa-github-username "BruceConnor")
;;     (setq lisa-name "Sir")
;;     (setq lisa-package-directories '("~/.emacs.d/packages/" "~/Git-Projects/"))
;;     (setq lisa-email "bruce.connor.am@gmail.com")
;;     (define-key lisa-mode-map (kbd "C-;") 'lisa-comment-sexp)
;;     (add-hook 'emacs-lisp-mode-hook 'lisa-mode)


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
;; 0.5 - 20130822 - Improved it a little more.
;; 0.5 - 20130822 - Improved change logs a little bit
;; 0.1 - 20130815 - Perfected keymap.
;; 0.1 - 20130815 - Greatly improved doc.
;; 0.1 - 20130815 - Started creating the minor mode.
;; 0.1 - 20130814 - Imported code.
;; 0.1 - 20130813 - Created File.
;;; Code:

(require 'yasnippet)
(defconst lisa-version "0.5" "Version of the lisa.el package.")
(defconst lisa-version-int 2 "Version of the lisa.el package, as an integer.")
(defun lisa-bug-report ()
  "Opens github issues page in a web browser. Please send me any bugs you find, and please inclue your emacs and lisa versions."
  (interactive)
  (browse-url "https://github.com/Bruce-Connor/lisp-assistant/issues/new")
  (message "Your lisa-version is: %s, and your emacs version is: %s.\nPlease include this in your report!"
           lisa-version emacs-version))
;; (defun lisa-customize ()
;;   "Open the customization menu in the `lisa' group."
;;   (interactive)
;;   (customize-group 'lisa t))

;;; ---------------------------------------------------------------------
;;; Package handling variables
(defvar lisa-package-name       nil "")
(defvar lisa-package-prefix     nil "")
(defvar lisa-package-version    nil "")
(defvar lisa-package-change-log ""  "")

(defcustom lisa-separator "-"
  "Separator to use after the prefix (`lisa-package-prefix').

This is usually a \"-\", but some people like to use \"/\" or
\":\"."
  :type 'string
  :group 'lisa
  :package-version '(lisa . "0.1"))

(defcustom lisa-github-username nil
  "Username Lisa uses to construct github urls inside templates.

Must be exactly as it appears in your github urls. For instance, here
   https://github.com/Bruce-Connor/lisp-assistant
the username would be \"Bruce-Connor\".

Leave this nil if you don't want to use it."
  :type 'string
  :group 'lisa
  :package-version '(lisa . "0.1"))

(make-variable-buffer-local 'lisa-package-name)
(make-variable-buffer-local 'lisa-package-prefix)
(make-variable-buffer-local 'lisa-package-version)
(make-variable-buffer-local 'lisa-separator)
(defcustom lisa-default-version-number "0.1a"
  "Initial version number for newly created packages."
  :type 'string
  :group 'lisa
  :package-version '(lisa . "0.1"))

;;; ---------------------------------------------------------------------
;;; Internal
(defcustom lisa-name (car (split-string (user-full-name)))
  "How should Lisa address you?" 
  :type 'string
  :group 'lisa
  :package-version '(lisa . "0.1"))

(defvar lisa--load-file-name load-file-name
  "Store the filename that lisa.el was originally loaded from.")
(defvar lisa--package-change-log ""  "")

(defconst lisa--success '("Happy to help." "Done." "Success." "Perfect." "Always a pleasure." "Neat." "All in order." "This looks promising." "Done." "No problem." "Here you go.") 
  "Always happy to help.")

(defun lisa--success ()
  "Always happy to help."
  (when lisa--success
    (message "%s" (nth
                   (random (length lisa--success))
                   lisa--success))))

(defun lisa--global-replace (str rep)
  "Version of replace-regexp usable in lisp code."
  (goto-char (point-min))
  (while (search-forward str nil t)
    (replace-match rep nil t)))

(defun lisa--format (sn &rest r)
  "Format the string to be polite."
  (unless lisa-name
    (setq lisa-name (car (split-string (user-full-name)))))
  (unless (and (stringp lisa-name)
               (< 0 (length lisa-name)))
    (setq lisa-name "Sir"))
  (replace-regexp-in-string "§" lisa-name (apply 'format sn r)))

(defun lisa--should-template ()
  "Lisa knows you want a template even if you don't ask.

Guess whether this is such a moment."
  (when (and lisa-package-directories
             (= (point-max) (point-min))
             (buffer-file-name))
    (unless (listp lisa-package-directories) 
      (error (lisa--format "I'm sorry §, but `lisa-package-directories' doesn't appear to be a list of directories.")))
    (= 0 (string-match (regexp-opt (mapcar 'expand-file-name lisa-package-directories))
                       (expand-file-name (buffer-file-name))))))

(defun lisa--insert-or-generate-package-template ()
  ""
  (let ((file lisa-package-template-file))
    (unless (file-readable-p file)
      (make-directory (file-name-directory (expand-file-name file)) :parents)
      (copy-file (lisa--original-template-file) file))
    (if (file-readable-p file)
        (insert-file-contents-literally file)
      (if (y-or-n-p (lisa--format "I'm sorry, §. For some reason I couldn't create the template file in %s.
I'll just use the one I have here, OK?" file))
          (insert-file-contents-literally (lisa--original-template-file))
        (message "I'm sorry I couldn't help. If you'd like to download the template yourself here's the URL:
https://raw.github.com/Bruce-Connor/lisp-assistant/master/template.elt")
        nil))))

(defun lisa--original-template-file ()
  "Guess the location of our local template file, from which we create the user's file."
  (concat (file-name-directory lisa--load-file-name) "lisa-package-template.elt"))

(defun lisa--umcomment-block ()         ;Thanks to Drew on http://stackoverflow.com/a/18294344/491532
  "Uncomment as many lines as possible."
  (let ((opoint  (point))
        beg end)
    (save-excursion
      (forward-line 0)
      (while (looking-at "^ *;+") (forward-line -1))
      (unless (= opoint (point))
        (forward-line 1)
        (setq beg  (point)))
      (goto-char opoint)
      (forward-line 0)
      (while (looking-at "^ *;+") (forward-line 1))
      (unless (= opoint (point))
        (setq end  (point)))
      (when (and beg  end)
        (uncomment-region beg end)
        (goto-char beg)
        (forward-sexp 1)
        (when (looking-at "[\n\r[:space:]]+)") 
          (delete-indentation 1))))))

;;; ---------------------------------------------------------------------
;;; Package handling functions
(defun lisa-insert-change-log (prefix &optional log)
  "Insert a change-log line at the header. Lisa will insert date and version number for you (and the final \".\", if you're lazy).

Lisa remembers everything you ask her to insert, so you can later
paste it all at once with by calling this with a PREFIX argument.

This is great for quickly using change-logs as commit messages in
Magit (or similar). Just insert change logs as they're made, and
when you're writting the commit message hit
M-x `lisa-insert-full-change-log' to have it all inserted for
you. (Or you could bind it to a key in your VC mode.)"
  (interactive "P")
  (let ((wasChanged (buffer-modified-p))
        neededDot)
    (when prefix (lisa-insert-full-change-log))
    (if (called-interactively-p 'interactive)
        (setq log (read-string (lisa--format "Of course. What did you change, §? "))))
    (push-mark)
    (goto-char 
     (save-excursion
       (goto-char (point-min))
       (unless (search-forward-regexp "^;;+ +Change Log: *\n" nil t)
         (goto-char (point-min))
         (unless (search-forward-regexp "^;;;+ +Code:" nil t)
           (error (lisa--format "I'm very sorry, §. I couldn't find the change-log nor the start of code.
Could you insert the string \";;; Change Log:\n\" somewhere?")))
         (forward-line -1)
         (insert ";;; Change Log:\n"))
       (insert "\n")
       (forward-char -1)
       (insert ";; " lisa-package-version " - "
               (format-time-string "%Y%m%d") 
               " - ")
       (save-excursion
         (insert log)
         (unless (looking-back "\.")
           (setq neededDot t)
           (insert ".")))
       (setq lisa-package-change-log
             (concat lisa-package-change-log "\n" log (if neededDot "." "")))
       (unless wasChanged (save-buffer))
       (point))))
  (lisa--success))

(defun lisa-insert-full-change-log ()
  "Insert under point all the change-log lines you previously inserted (with `lisa-insert-change-log').

This is great for quickly using change-logs as commit messages in
Magit (or similar). You might want it to be called automatically
when you're editing a commit, just do:

    (add-hook 'git-commit-mode-hook 'lisa-insert-full-change-log)

Note this hook only exists in newer versions of Magit."
  (interactive)
  (unless (= 0 (length lisa-package-change-log))
    (setq lisa--package-change-log lisa-package-change-log)
    (setq lisa-package-change-log  ""))
  (insert lisa--package-change-log)
  (lisa--success))

(defun lisa-update-version-number ()
  "Ask Lisa to change the version number of current package.

Edits the comment at the header, and updates any defconst's
containing the version number."
  (interactive)
  (let ((vo lisa-package-version))
    (setq lisa-package-version (read-string (lisa--format "What's the new number, §? ") vo))
    (goto-char (point-min))
    (if (search-forward (concat ";; Version: " vo) nil t)
        (replace-match (concat ";; Version: " lisa-package-version))
      (setq lisa-package-version vo)
      (error "Couldn't find ';; Version:' comment. `lisa-package-version' variable not updated."))
    (if (search-forward-regexp
         (concat "(defconst *" lisa-package-prefix lisa-separator "version *\"\\(" vo "\\)")
         nil t)
        (replace-match lisa-package-version nil nil nil 1)
      ;; (warn "Couldn't find version 'defconst'.")
      )
    (if (search-forward-regexp
         (concat "(defconst *" lisa-package-prefix lisa-separator "version-int *\\([0-9]+\\)")
         nil t)
        (replace-match (int-to-string (1+ (string-to-number (match-string 1))))
                       nil nil nil 1)))
  (lisa--success))

(defun lisa-define-package-variables ()
  "Guess package-specific variables from file name and content.

These variables are `lisa-package-prefix', `lisa-package-version',
`lisa-package-name', and `lisa-separator'.

These are variables of which Lisa keeps track for you, so they
can be used in yasnippets or in your own customization code, and
Lisa can update them for you (when the version number changes).

The package name and version are used in the \"defcustom\"
snippet, invokable by typing \"(dc\" then \\[yas-expand].
The prefix and separator are also used with the \"defun\"
snippet, invokable by typing \"(df\" then \\[yas-expand].

The first three can also be quickly inserted with yasnippets.
Just type \"pp\", \"pv\", or \"pn\", and hit \\[yas-expand] (depends
on yas-minor-mode being active)."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (setq lisa-package-name (if (search-forward-regexp "^(provide '\\([^)]+\\))" nil t)
                                (match-string-no-properties 1)
                              (file-name-base (or (buffer-file-name)
                                                  (buffer-name) ""))))
    (goto-char (point-min))
    (setq lisa-package-prefix
          (if (search-forward-regexp "^;;+ +\\(Prefix\\|ShortName\\): +\\([-/:a-zA-Z0-9\.]+\\)" nil t)
              (match-string-no-properties 1)
            (mapconcat (lambda (s) (substring s 0 1))
                       (split-string lisa-package-name "-" :omit-nulls)
                       "")))
    (goto-char (point-min))
    (setq lisa-package-version
          (if (search-forward-regexp "^;;+ +Version: +\\([0-9a-zA-Z\.]+\\)" nil t)
              (match-string-no-properties 1) nil))
    (goto-char (point-min))
    (if (search-forward-regexp "^;;+ +Separator: +\\([^ \n',`(){}]+\\)" nil t)
        (setq lisa-separator (match-string-no-properties 1)))))

;;; ---------------------------------------------------------------------
;;; General coding 
(defcustom lisa-snippets-close-paren t
  "Should Lisa close parens after expanding her special snippets?

This should usually always be t.
This is ignored (treated as nil) if paredit mode is active."
  :type 'boolean
  :group 'lisa
  :package-version '(lisa . "0.1"))

(defun lisa-find-or-define-function ()
  "Look at the symbol under point. If it's a defined function, go to it. If it isn't, go back to top level code and create a function with this name.

This meant to aid your workflow. If you write in your code the
name of a function you haven't defined yet, you can then just
place point on its name and hit \\[lisa-find-or-define-function] and a yasnippet
defun will be inserted with point inside.

If the function under point is already defined this just calls
`find-function'."
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
  "Look at the symbol under point. If it's a defined variable, go to it. If it isn't, go back to top level code and create a variable with this name.
This meant to aid your workflow. If you write in your code the
name of a variable you haven't defined yet, you can then just
place point on its name and hit \\[lisa-find-or-define-variable] and a yasnippet
defun will be inserted with point inside.

If the variable under point is already defined this just calls
`find-variable'."
  (interactive)
  (if (symbolp (variable-at-point))
      (find-variable (variable-at-point))
    (push-mark)
    (let ((name (thing-at-point 'symbol)))
      (beginning-of-defun)
      (when (looking-back "^;;;###autoload\\s-*\n")
        (forward-line -1))
      (insert "(dc)\n\n")
      (backward-char 3)
      (yas-expand)
      (insert name))))

(defun lisa-comment-sexp ()
  "Move to beginning of current sexp and comment it.

If at the beginning of a comment, uncomment it (though this part
still needs improving)."
  (interactive) ;;If this whole line is a comment.
  (if (string-match "^ *;+" (buffer-substring-no-properties (line-beginning-position) (line-end-position))) 
      (lisa--umcomment-block)
    (when (eq (or (get-char-property (point) 'read-face-name)
                  (get-char-property (point) 'face))
              'font-lock-comment-face)
      (indent-for-comment))
    (unless (looking-at "(")
      (lisa--backward-up-sexp))
    (mark-sexp)
    (comment-region (region-beginning) (region-end))))

(defun lisa--backward-up-sexp ()
  "Up goes the road to success."
  (condition-case nil
      (if (fboundp 'paredit-backward-up)
          (paredit-backward-up)
        (backward-up-list))
    (error (forward-sexp -1))))


;;; ---------------------------------------------------------------------
;;; Lisa Keymap
(define-prefix-command 'lisa-lisa-map)
(define-prefix-command 'lisa-eval-map)
(define-key lisa-eval-map "b" 'eval-buffer)
(define-key lisa-eval-map "d" 'eval-defun)
(define-key lisa-eval-map "e" 'eval-expression)
(define-key lisa-eval-map "l" 'eval-last-sexp)
(define-key lisa-eval-map "p" 'eval-print-last-sexp)
(define-key lisa-eval-map "r" 'eval-region)
(define-key lisa-lisa-map "n" 'lisa-define-package-variables)
(define-key lisa-lisa-map "#" 'lisa-define-package-variables)
(define-key lisa-lisa-map "l" 'lisa-insert-change-log)
(define-key lisa-lisa-map "t" 'lisa-insert-template)
(define-key lisa-lisa-map "u" 'lisa-update-version-number)
(define-key lisa-lisa-map "f" 'lisa-find-or-define-function)
(define-key lisa-lisa-map "v" 'lisa-find-or-define-variable)
(define-key lisa-lisa-map "c" 'lisa-comment-sexp)
(defcustom lisa-helpful-keymap nil
  "Whether Lisa's keymap should prefer being helpful or unobstrusive.

If this is nil, Lisa will prefer to stay out of your way. The
lisa key bindings will be under \"C-c l\" (use \"C-c l C-h\" to
see them all), and the eval key bindings will be under \"C-c
e\" (use \"C-c e C-h\" to see them all).

If this is t, Lisa will always be right around the corner. All
key bindings will be directly under \"C-c\".
By default, these will be:
    \"C-c f\" `lisa-find-or-define-function'
    \"C-c v\" `lisa-find-or-define-variable'.
    \"C-c #\" `lisa-define-package-variables'
    \"C-c l\" `lisa-insert-change-log'
    \"C-c u\" `lisa-update-version-number'
    \"C-c t\" `lisa-insert-template'
    \"C-c b\" `eval-buffer'
    \"C-c d\" `eval-defun'
    \"C-c e\" `eval-expression'
    \"C-c p\" `eval-print-last-sexp'
    \"C-c r\" `eval-region'

This can also be 'eval (or 'lisa). In that case only the
eval-something functions (or the lisa-something functions,
respectively) will be bound under \"C-c\", the others will stay
under \"C-c l\" (or \"C-c e\", respectively)."
  :type 'boolean
  :group 'lisa
  :package-version '(lisa . "0.1"))

;;; ---------------------------------------------------------------------
;;; Templating
(defcustom lisa-package-template-file (concat user-emacs-directory "lisa-package-template.elt")
  "File which Lisa will use as template for new packages.

If file isn't found, Lisa creates it for you."
  :type 'file
  :group 'lisa
  :package-version '(lisa . "0.1"))

(defcustom lisa-package-directories nil
  "If this is nil, auto-insertion of templates is disabled.

Otherwise it should be a list of directory names. Any new \".el\"
file created inside one of these directories (or its subtree)
will be automatically populated with a package template, which
Lisa will then proceed to fill out for you.

Even if this is nil you can still insert templates with
\\[lisa-insert-template].

For instance, if the value is '(\"~/.emacs.d/packages/\"),
whenever you open an empty file (with a .el extension) inside
that folder the function `lisa-insert-template' will be called
automatically.

IMPORTANT! Make sure your elpa directory (usually
\"~/.emacs.d/elpa/\") is NOT inside this. Lisa is a very nice
lady, but if you don't follow this warning she'll turn into a hag
every time you install/update packages. "
  :type 'directory
  :group 'lisa
  :package-version '(lisa . "0.1"))

(defcustom lisa-full-name (user-full-name)
  "Your full name, to be used when filling templates."
  :type 'string
  :group 'lisa
  :package-version '(lisa . "0.1"))

(defcustom lisa-email user-mail-address
  "Your email address, to be used when filling templates."
  :type 'string
  :group 'lisa
  :package-version '(lisa . "0.1"))

(defun lisa-insert-template ()
  "Lisa inserts the template for a new package, asks a couple of details and fills them in for you.

The best way to understand this function is to call it inside an
empty buffer. Lisa will insert the contents of
`lisa-package-template-file' and fill in the details about the
package. Some details can be guessed (like the name) some will be
asked.

Feel free to edit that template to your liking. If the file
doesn't exist, she will kindly offer to download it for you."
  (interactive)
  (let ((yr (format-time-string "%Y"))
        (dt (format-time-string "%Y%m%d")))
    ;; Try to guess the variables
    (lisa-define-package-variables)
    ;; Insert and fill in the template
    (when (lisa--insert-or-generate-package-template)
      ;; Check if the prefix is what the user wants
      (setq lisa-package-prefix 
            (read-string (lisa--format "§, what is the package prefix? (tipically 2-4 letters) ")
                         lisa-package-prefix))
      (when (string= lisa-package-prefix "")
        (setq lisa-package-prefix nil))
      ;; Ask for an initial version number
      (unless lisa-package-version
        (setq lisa-package-version
              (read-string "Is this the version number you wanted? "
                           lisa-default-version-number)))
      (lisa--global-replace "___full_name___" lisa-full-name)
      (lisa--global-replace "___email___" lisa-email)
      (lisa--global-replace "___github_user_name___" lisa-github-username)
      (lisa--global-replace "___package_name___" lisa-package-name)
      (lisa--global-replace "___version___" lisa-package-version)
      (lisa--global-replace "___year___" yr)
      (lisa--global-replace "___date___" dt)
      (lisa--global-replace "___prefix___" (or lisa-package-prefix lisa-package-name))
      (lisa--global-replace "___sep___" lisa-separator)
      (goto-char (point-min))
      (goto-char (line-end-position))
      (insert (or (read-string "Would you like to write a short description? ") "")) 
      (search-forward "; Keywords: ")
      (message "%s" (lisa--format "Thank you, §. Call me if you need anything.")))))

;;;###autoload
(define-minor-mode lisa-mode
  "Meet Lisa, your Lisp Assistant.
lisa-mode defines a few functions, keybindings, buffer-local
variables, and yasnippet templates to aid in developing lisp
code.

The usual way to turn it on is:
    (add-hook 'emacs-lisp-mode-hook 'lisa-mode)

Most useful functions are (see full keymap below for other functions):
\\[lisa-insert-change-log]  `lisa-insert-change-log'
\\[lisa-update-version-number]  `lisa-update-version-number'
\\[lisa-find-or-define-function]  `lisa-find-or-define-function'
\\[lisa-find-or-define-variable]  `lisa-find-or-define-variable'

Most useful yasnippets are:
   (df ---> Expands to a full defun
   (dc ---> Expands to a full defcustom
 (for the others, read after the keymap description below).

These two snippets make use of some package variables (see the doc
of `lisa-define-package-variables' for more information). These
variables make the snippets super useful, and they are
automatically defined whenever you open an existing .el file or
create one from a template. If you create your file from scratch,
these won't be automatically defined, but you can use
\\[lisa-define-package-variables] to redefine them.

\\{lisa-mode-map}.

snippet         expansion
-------         ---------
 (df            (defun ...)
 (dc            (defcustom ...)
 (dm            (defmacro ...)
 (da            (defadvice ...)
 (dg            (defgroup ...)
 (dk            (define-key ...)
 (dv            (defvar ...)
 (dco           (defconst ...)
 (dfa           (defface ...)
"
  nil " Lisa"
  '(("l" . lisa-lisa-map)
    ("e" . lisa-eval-map)
    ([3 67108923] . lisa-comment-sexp)) ;C-c C-;
  :global nil
  :group 'lisa 
  (if lisa-mode
      (progn
        ;; Activate yasnippets
        (when (and lisa--load-file-name (fboundp 'yas-reload-all))
          (let ((dir (concat (file-name-directory lisa--load-file-name) "snippets")))
            (when (file-directory-p dir)
              (unless (member dir yas-snippet-dirs)
                ;; Append so where're not treated as user library
                (add-to-list 'yas-snippet-dirs dir t)
                (add-hook 'yas-after-exit-snippet-hook 'lisa--insert-colosing-paren-before-expand)
                (yas-reload-all)))))
        ;; Keymap
        ;; (setq lisa-mode-map '(keymap))
        (define-key lisa-mode-map "l" lisa-lisa-map)
        (define-key lisa-mode-map "e" lisa-eval-map)
        (when (or (eq lisa-helpful-keymap 'eval)
                  (eq lisa-helpful-keymap t))
          (define-key lisa-mode-map "b" 'eval-buffer)
          (define-key lisa-mode-map "d" 'eval-defun)
          (define-key lisa-mode-map "e" 'eval-expression)
          ;; (define-key lisa-mode-map "l" 'eval-last-sexp)
          (define-key lisa-mode-map "p" 'eval-print-last-sexp)
          (define-key lisa-mode-map "r" 'eval-region))
        (when (or (eq lisa-helpful-keymap 'lisa)
                  (eq lisa-helpful-keymap t))
          (define-key lisa-mode-map "#" 'lisa-define-package-variables)
          (define-key lisa-mode-map "l" 'lisa-insert-change-log)
          (define-key lisa-mode-map "u" 'lisa-update-version-number)
          ;; (define-key lisa-mode-map "c" 'lisa-comment-sexp)
          (define-key lisa-mode-map "t" 'lisa-insert-template)
          (define-key lisa-mode-map "f" 'lisa-find-or-define-function)
          (define-key lisa-mode-map "v" 'lisa-find-or-define-variable))
        ;; New package template
        (if (lisa--should-template)
            (lisa-insert-template)
          (lisa-define-package-variables)))))

(defun lisa--insert-colosing-paren-before-expand ()
  "Runs in `yas-before-expand-snippet-hook'."
  (when (and lisa-mode
             lisa-snippets-close-paren
             (not (and (boundp 'paredit-mode) paredit-mode)))
    (insert ")")
    (forward-char -1)))

(provide 'lisa)
;;; lisa.el ends here.
