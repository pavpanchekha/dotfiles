(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime")
(require 'slime)
(setq slime-backend "~/.emacs.d/slime/loader.lsp"
      inferior-lisp-program "/usr/bin/sbcl")
(slime-setup)
