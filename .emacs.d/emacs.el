
(setq user-full-name "Pavel Panchekha")
(setq user-mail-address "me@pavpanchekha.com")

(add-to-list 'load-path "~/.emacs.d/load-path")

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(setq tetris-score-file "~/.emacs.d/scores/tetris")
(setq snake-score-file  "~/.emacs.d/scores/snake")

(setq make-backup-files nil)
(setq-default indent-tabs-mode nil)
(setq require-final-newline t)
(fset 'yes-or-no-p 'y-or-n-p)

(setq search-highlight t)
(setq query-replace-highlight t)
(setq case-fold-search t)

(setq scroll-conservatively 5)
(setq scroll-margin 5)

(add-hook 'server-switch-hook
          (lambda nil
            (let ((server-buf (current-buffer)))
              (bury-buffer)
              (switch-to-buffer-other-frame server-buf))))
(add-hook 'server-done-hook
          (lambda ()
            (delete-frame)
            (kill-buffer nil)))

(setq viper-inhibit-startup-message t)
(setq viper-expert-level 5)
(setq viper-mode t)

(require 'viper)

(require 'vimpulse)

(windmove-default-keybindings 'hyper)

(global-set-key [C-m] 'newline-and-indent)
(global-set-key [delete] 'delete-char)
(setq kill-whole-line t)

(defun run-command ()
  (interactive)
  (shell-command (concat "run " (buffer-file-name) " &")))

(defun compile-command ()
  (interactive)
  (shell-command (concat "run -c " (buffer-file-name) " &")))

(global-set-key (kbd "<f5>") 'run-command)
(global-set-key (kbd "C-<f5>") 'compile-command)

(require 'doc-view)
(define-key doc-view-mode-map (kbd "j") 'doc-view-next-line-or-next-page)
(define-key doc-view-mode-map (kbd "k") 'doc-view-previous-line-or-previous-page)
(define-key doc-view-mode-map (kbd "h") 'image-backward-hscroll)
(define-key doc-view-mode-map (kbd "l") 'image-forward-hscroll)

(add-to-list 'auto-mode-alist '("\\.txt$" . org-mode))

(defun text-minor-modes ()
  (interactive)
  (auto-fill-mode)
  (flyspell-mode))

(add-hook 'text-mode-hook 'text-minor-modes)
(add-hook 'LaTeX-mode-hook 'text-minor-modes)
(add-hook 'org-mode-hook 'text-minor-modes)

(setq ispell-program-name "/usr/bin/ispell")
(setq ispell-dictionary "american")
(setq ispell-personal-dictionary "~/.emacs.d/dict")

(load "auctex.el" nil t t)
(load "preview-latex.el" nil t t)

(setq load-path
      (append load-path
              '("/usr/share/emacs/site-lisp/clojure-mode"
                "/usr/share/emacs/site-lisp/haskell-mode")))

(require 'haskell-mode)
(require 'clojure-mode)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/slime")
(require 'slime)

(setq slime-backend "~/.emacs.d/slime/loader.lsp")
(setq inferior-lisp-program "/usr/bin/sbcl")

(slime-setup)

(require 'org-install)

(setq org-modules '(org-bibtex org-docview org-info org-jsinfo org-irc
                    org-rmail org-w3m org-eshell))

(setq org-directory "~/notes/")
(setq org-agenda-files '("~/notes/"))
(setq org-default-notes-file (concat org-directory "pavel.org"))

(setq org-M-RET-may-split-line '((default)))

(setq org-agenda-ndays 7)
(setq org-agenda-show-all-dates t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-start-on-weekday nil)

(setq org-todo-keywords '(
        (sequence "TODO" "|" "DONE" "WAIT" "WONT")
        (sequence "SOMEDAY" "|" "DONE")
        (sequence "TODO" "READY" "CODED" "TESTED" "FIXED" "DONE")))

(setq org-pretty-entities t
      org-entities-user '(("CC" "\\CC" t "&#8450;" "C" "C" "ℂ")
                          ("FF" "\\FF" t "&#120125;" "F" "F" "𝔽")
                          ("HH" "\\HH" t "&#8461;" "H" "H" "ℍ")
                          ("NN" "\\NN" t "&#8469;" "N" "N" "ℕ")
                          ("PP" "\\PP" t "&#8473;" "P" "P" "ℙ")
                          ("QQ" "\\QQ" t "&#8474;" "Q" "Q" "ℚ")
                          ("RR" "\\RR" t "&#8477;" "R" "R" "ℝ")
                          ("ZZ" "\\ZZ" t "&#8484;" "Z" "Z" "ℤ")))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)

(require 'magit)
(autoload 'magit-status "magit" nil)

(setq doc-view-continuous t)

(setq doc-view-resolution 192)

(require 'w3m)

(setq w3m-coding-system 'utf-8
      w3m-default-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8)

(setq w3m-default-save-directory "/tmp/")
(setq w3m-icon-directory "/tmp/")

(setq w3m-use-cookies t w3m-use-favicon t)

(setq w3m-home-page "about:blank")

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)

(global-set-key "\C-xm" 'browse-url-at-point)

(define-key w3m-mode-map (kbd "f") 'w3m-view-this-url)
(define-key w3m-mode-map (kbd "F") 'w3m-view-this-url-new-session)

(defun clean-slate-goto-url (url)
  (interactive (list (w3m-input-url nil "" nil nil 'feeling-lucky)))
  (w3m-goto-url url))

(define-key w3m-mode-map (kbd "o")   'clean-slate-goto-url)
(define-key w3m-mode-map (kbd "O")   'w3m-goto-url)
(define-key w3m-mode-map (kbd "C-o") 'w3m-view-previous-page)

(defun clean-slate-goto-url-new-session (url)
  (interactive (list (w3m-input-url nil "" nil nil 'feeling-lucky)))
  (w3m-goto-url-new-session url))

(define-key w3m-mode-map (kbd "t") 'clean-slate-goto-url-new-session)
(define-key w3m-mode-map (kbd "T") 'w3m-goto-url-new-session)

(define-key w3m-mode-map (kbd "d") 'w3m-delete-buffer)

(define-prefix-command 'vim-tab-switching)
(define-key w3m-mode-map (kbd "g") 'vim-tab-switching)

(define-key vim-tab-switching "t" 'w3m-next-buffer)
(define-key vim-tab-switching "T" 'w3m-previous-buffer)

(define-key w3m-mode-map "/" 'isearch-forward)

(add-to-list 'load-path "~/.emacs.d/load-path/")
(require 'rmail-extras)

(setq rmail-default-file "~/mail/")
(setq rmail-file-name "~/mail/inbox.spool")
(setq rmail-preserve-inbox t)
(setq mail-default-directory "~/mail/")

(setq sendmail-program "/usr/bin/msmtp")

(add-hook 'message-mode-hook
          (lambda ()
            (add-to-list 'message-hidden-headers "^In-Reply-To:")
            (turn-on-orgstruct++)))

(setq message-directory "~/mail/")
(setq mml-default-directory "~/mail/")

(setq rmail-display-summary t)

(setq rmail-displayed-headers "^To:\\|From:\\|Date:\\|Subject:")

(setq rmail-summary-window-size 10)
(setq rmail-summary-scroll-between-messages nil)

(add-hook 'rmail-show-message-hook (lambda () (goto-address-mode 1)))

(defun rmail-archive ()
  (interactive)
  (rmail-output "~/mail/archive.spool")
  (rmail-delete-forward))

(defun rmail-toggle-view-archive ()
  (interactive)
  (delete-other-windows)
  (if (or (string= (buffer-name) "inbox.spool")
          (string= (buffer-name) "inbox.spool-summary"))
    (rmail-input "~/mail/archive.spool")
    (rmail-input "~/mail/inbox.spool")))

(define-key rmail-mode-map (kbd "d") 'rmail-archive)
(define-key rmail-mode-map (kbd "D") 'rmail-delete-forward)
(define-key rmail-mode-map (kbd "I") 'rmail-toggle-view-archive)

(add-hook 'rmail-mode-hook
          (lambda ()
            (define-key rmail-summary-mode-map (kbd "d") 'rmail-archive)
            (define-key rmail-summary-mode-map (kbd "D") 'rmail-delete-forward)
            (define-key rmail-summary-mode-map (kbd "I") 'rmail-toggle-view-archive)))

(setq eshell-directory-name "~/.emacs.d/eshell/")

(require 'multi-term)
(setq multi-term-program "/usr/bin/fish")
(add-to-list 'term-unbind-key-list "C-w")
