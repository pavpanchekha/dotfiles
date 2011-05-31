;; All it does is initialize org-babel, which stores the rest of my initialization files.

(require 'org-install)
(require 'ob-tangle)
(mapc #'org-babel-load-file (directory-files "~/.emacs.d/" t "\\.org$"))
