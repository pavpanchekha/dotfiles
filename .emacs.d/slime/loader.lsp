(load "/usr/share/emacs/site-lisp/slime/swank-loader.lisp")
(cl:in-package :swank-loader)
(setf *fasl-directory* (merge-pathnames
                        (make-pathname
                         :directory `(:relative ".emacs.d/slime" "fasl"
                                                ,@(if (slime-version-string)
                                                      (list (slime-version-string)))
                                                ,(unique-dir-name)))
                        (user-homedir-pathname)))
