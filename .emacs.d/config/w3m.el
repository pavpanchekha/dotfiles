
(define-key w3m-mode-map (kbd "o")   'w3m-goto-url)
(define-key w3m-mode-map (kbd "d")   'w3m-delete-buffer)
(define-key w3m-mode-map (kbd "t")   'w3m-goto-url-new-session)

(define-key w3m-mode-map (kbd "f")   'w3m-view-this-url)
(define-key w3m-mode-map (kbd "F")   'w3m-view-this-url-new-session)

(define-prefix-command 'vim-tab-switching)
(define-key w3m-mode-map (kbd "g") 'vim-tab-switching)

(define-key vim-tab-switching "t" 'w3m-next-buffer)
(define-key vim-tab-switching "T" 'w3m-previous-buffer)
(define-key w3m-mode-map (kbd "/")   'w3m-search)
