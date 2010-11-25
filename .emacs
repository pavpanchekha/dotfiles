;; A bit about me
(setq user-mail-address "pavpanchekha@gmail.com")
(setq user-full-name "Pavel Panchekha")

;; Color Theme
(require 'color-theme)
(color-theme-initialize)
(color-theme-dark-laptop)

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
(global-set-key "\C-m" 'newline-and-indent) ; Newline indents
(global-set-key [delete] 'delete-char)      ; [Delete] deletes
(setq kill-whole-line t)                    ; C-k at the start of the line kills the whole line

;; Modes
(setq auto-mode-alist
      (append '(("\\.h$" . c++-mode) ("\\.txt$" . rst-mode) ("\\.cl$" . common-lisp-mode))
              auto-mode-alist))
(add-hook 'rst-mode 'auto-fill-mode)
(add-hook 'LaTeX-mode 'auto-fill-mode)
(add-hook 'rst-mode ' flyspell-mode)
(setq rst-level-face-base-light 20)

; Flymake breaks latex


;; Interface
(custom-set-variables
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode t)
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))


;; My run command
(defun run* ()
  (interactive)
  (shell-command (concat "run " (buffer-file-name))))

(defun compile* ()
  (interactive)
  (shell-command (concat "run -c " (buffer-file-name))))

(global-set-key (kbd "<f5>") 'run*)
(global-set-key (kbd "C-<f5>") 'compile*)

;; Some shortcuts
(defun open-todo ()
  (interactive)
  (find-file "~/notes/todo.org"))

;; Using gnome-keyring support (through Python!)
(pymacs-load "keyring" "keyring-")

; ERC gets keyring lovin'
(setq erc-password (keyring-get-password "ERC" "pavpanchekha"))
(setq erc-prompt-for-password nil)

(setq w3m-use-cookies t)


(setq tetris-score-file "~/.emacs.d/scores/tetris"
      snake-score-file "~/.emacs.d/scores/snake")

;; Emacs with Mutt
(add-to-list 'auto-mode-alist '("mutt-" . mail-mode))

;; Better term colors
(setq ansi-color-names-vector ; better contrast colors
      ["black" "red4" "green4" "yellow4"
       "blue7" "magenta4" "cyan4" "white"])
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

;; Emacsclient/server hook
(add-hook 'server-switch-hook 
          (lambda ()
            (when (current-local-map)
              (use-local-map (copy-keymap (current-local-map))))
            (local-set-key (kbd "C-x k") 'server-edit)))

;(load-file "~/.emacs.d/config/jabber.el")
(load-file "~/.emacs.d/config/org.el")
(load-file "~/.emacs.d/config/notify.el")
(load-file "~/.emacs.d/config/c-mode.el")
(load-file "~/.emacs.d/config/rmail.el")
(load-file "~/.emacs.d/config/vim.el")
(load-file "~/.emacs.d/config/tabbar.el")
(load-file "~/.emacs.d/config/python.el")
