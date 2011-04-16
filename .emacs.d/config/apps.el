(require 'cl)

(defmacro* defapp (keybinding &rest cmd)
  (let ((name (gensym)))
    `(progn
       (defun ,name ()
         (interactive)
         (select-frame (new-frame))
         ,@cmd)
       (global-set-key (kbd ,keybinding) ',name))))

(defapp "s-X" (eshell))
(defapp "s-M" (rmail))
(defapp "s-L" (slime))
(defapp "s-J" (jabber-switch-to-roster-buffer))
(defapp "s-Z" (w3m))
(defapp "s-G" (magit-status "~/"))
