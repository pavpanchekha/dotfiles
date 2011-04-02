(custom-set-variables
 '(slime-backend "~/.emacs.d/slime/loader.lsp"))

(setq inferior-lisp-program "/usr/bin/sbcl")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime")
(require 'slime)
(slime-setup)
