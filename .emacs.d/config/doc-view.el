(require 'doc-view)

(setq doc-view-continuous t
      doc-view-resolution 192)

(define-key doc-view-mode-map (kbd "j") 'doc-view-next-line-or-next-page)
(define-key doc-view-mode-map (kbd "k") 'doc-view-previous-line-or-previous-page)
(define-key doc-view-mode-map (kbd "h") 'image-backward-hscroll)
(define-key doc-view-mode-map (kbd "l") 'image-forward-hscroll)
