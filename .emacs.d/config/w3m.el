(require 'w3m)

; Some variables
(setq w3m-coding-system 'utf-8
      w3m-default-coding-system 'utf-8
      w3m-file-coding-system 'utf-8
      w3m-file-name-coding-system 'utf-8
      w3m-terminal-coding-system 'utf-8
      
      w3m-default-display-inline-images t
      w3m-default-save-directory "/tmp"
      w3m-favicon-size '(16 . 16)
      w3m-home-page "about:blank"
      w3m-icon-directory "/tmp"
      w3m-init-file "~/.emacs.d/w3m.el"
      w3m-key-binding nil
      w3m-use-favicon t)

(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
(global-set-key "\C-xm" 'browse-url-at-point)

(defun clean-slate-goto-url (url)
  (interactive (list (w3m-input-url nil "" nil nil 'feeling-lucky)))
  (w3m-goto-url url))

(defun clean-slate-goto-url-new-session (url)
  (interactive (list (w3m-input-url nil "" nil nil 'feeling-lucky)))
  (w3m-goto-url-new-session url))

(define-key w3m-mode-map (kbd "o")   'clean-slate-goto-url)
(define-key w3m-mode-map (kbd "O")   'w3m-goto-url)
(define-key w3m-mode-map (kbd "C-o") 'w3m-view-previous-page)
(define-key w3m-mode-map (kbd "d")   'w3m-delete-buffer)
(define-key w3m-mode-map (kbd "t")   'clean-slate-goto-url-new-session)
(define-key w3m-mode-map (kbd "T")   'w3m-goto-url-new-session)

(define-key w3m-mode-map (kbd "f")   'w3m-view-this-url)
(define-key w3m-mode-map (kbd "F")   'w3m-view-this-url-new-session)

(define-prefix-command 'vim-tab-switching)
(define-key w3m-mode-map (kbd "g")   'vim-tab-switching)

(define-key vim-tab-switching "t"    'w3m-next-buffer)
(define-key vim-tab-switching "T"    'w3m-previous-buffer)
(define-key vim-tab-switching "g"    'beginning-of-buffer)
(define-key w3m-mode-map (kbd "/")   'isearch-forward)
