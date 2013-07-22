;; All it does is initialize org-babel, which stores the rest of my initialization files.

(package-initialize)
(require 'ob-tangle)
(org-babel-load-file "~/.emacs.d/emacs.org")
