Meet Lisa, your Lisp Assistant. ![test](https://checkout.google.com/buttons/support.gif?merchant_id=081240517764473&w=130&h=50&style=trans&variant=text)
====

`lisa-mode` is a minor-mode which defines a series of features to
aid in lisp development. It defines:
  1. a few useful functions,
  2. keybindings for these functions,
  3. buffer-local variables which contain package data (package name,
     version number, etc),
  4. yasnippet templates to aid in developing lisp code.
  
[You should really whatch this screencast to understand how she can
help you.]()

Most useful functions are: 
  1. `lisa-insert-change-log`
  2. `lisa-update-version-number`
  3. `lisa-find-or-define-function`
  4. `lisa-find-or-define-variable`
(see the `lisa-mode` function description for the others)

Most useful yasnippets are:

    (df ---> Expands to a full defun
    (dc ---> Expands to a full defcustom
(see the `lisa-mode` function description for the others)

These two snippets make use of some package variables (see the doc
of `lisa-define-package-variables` for more information). These
variables make the snippets super useful, and they are
automatically defined whenever you open an existing .el file or
create one from a template. If you create your file from scratch,
these won't be automatically defined, but you can use
`lisa-define-package-variables` to redefine them.

Installation
==============

The most basic installation process it to contact Lisa and then hire her:

    (require 'lisa)
    (add-hook 'emacs-lisp-mode-hook 'lisa-mode)

This will define several useful functions, the lisa-mode-map, and
the snippets.

The best way to learn about all of these is to read the description
of lisa-mode (C-h f lisa-mode RET).

To get the most out of Lisa, you need to give her a bit of data to
work with, so here's a sample configuration:

    (require 'lisa)
    (setq lisa-helpful-keymap t)
    (setq lisa-github-username "BruceConnor")
    (setq lisa-name "Sir")
    (setq lisa-package-directories '("~/.emacs.d/packages/" "~/Git-Projects/"))
    (setq lisa-email "bruce.connor.am@gmail.com")
    (define-key lisa-mode-map (kbd "C-;") 'lisa-comment-sexp)
    (add-hook 'emacs-lisp-mode-hook 'lisa-mode)

