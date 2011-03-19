;; A bit about me
(setq user-mail-address "pavpanchekha@gmail.com")
(setq user-full-name "Pavel Panchekha")

;; Some Standardization
(setq make-backup-files nil)        ; Stupid file~
(setq require-final-newline t)      ; Files must end in a newline
(setq-default indent-tabs-mode nil) ; Tabs should not be used

;; Searching
(setq case-fold-search t) ; Case-insensitive search
(setq search-highlight t) ; Highlight when I search
(setq query-replace-highlight t) ; Highlight when I search-and-replace
(fset 'yes-or-no-p 'y-or-n-p) ; Prompt for single characters

;; Scrolling
(setq scroll-conservatively 5) ; Scroll at most 5 lines at a time
(setq scroll-margin 5) ; Keep at least five lines around point

;; Key Bindings
(global-set-key [C-m] 'newline-and-indent) ; Newline indents
(global-set-key [delete] 'delete-char)      ; [Delete] deletes
(setq kill-whole-line t)                    ; C-k at the start of the line kills the whole line

;; Modes
(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode)
		("\\.txt$" . rst-mode)
		("\\.cl$" . common-lisp-mode))
              auto-mode-alist))
(add-hook 'rst-mode 'auto-fill-mode)
(add-hook 'rst-mode 'flyspell-mode)

(add-hook 'LaTeX-mode-hook 'auto-fill-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; My run command
(defun run* ()
  (interactive)
  (shell-command (concat "run " (buffer-file-name) " &")))

(defun compile* ()
  (interactive)
  (shell-command (concat "run -c " (buffer-file-name) " &")))

(global-set-key (kbd "<f5>") 'run*)
(global-set-key (kbd "C-<f5>") 'compile*)

;; w3m

(setq w3m-use-cookies t)

;; Score files
(setq tetris-score-file "~/.emacs.d/scores/tetris"
      snake-score-file  "~/.emacs.d/scores/snake")

;; Emacs with Mutt
(add-to-list 'auto-mode-alist '("mutt-" . mail-mode))

;; Emacsclient/server hook
(add-hook 'server-switch-hook 
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (local-set-key (kbd "C-x k") 'server-edit)))

(load-file "~/.emacs.d/config/doc-view.el")
(load-file "~/.emacs.d/config/eshell.el")
(load-file "~/.emacs.d/config/ispell.el")
(load-file "~/.emacs.d/config/rmail.el")
(load-file "~/.emacs.d/config/org.el")
(load-file "~/.emacs.d/config/w3m.el")
(load-file "~/.emacs.d/config/vim.el")
(load-file "~/.emacs.d/config/jabber.el")
(load-file "~/.emacs.d/config/slime.el")
(autoload 'magit-status "magit" nil)
(load-file "~/.emacs.d/config/newsticker.el")
(load-file "~/.emacs.d/config/apps.el")
