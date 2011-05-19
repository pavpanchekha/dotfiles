(add-to-list 'load-path "~/.emacs.d/load-path/")
(require 'rmail-extras)

(setq rmail-default-body-file "/tmp/pavpanchekha-compose"
      rmail-default-file "~/mail/"
      rmail-file-name "~/mail/inbox.spool"
      rmail-preserve-inbox t
      rmail-redisplay-summary t

      rmail-mime-show-images nil
      rmail-displayed-headers "^To:\\|From:\\|Date:\\|Subject:"

      rmail-display-summary t
      rmail-summary-window-size 5
      rmail-summary-scroll-between-messages nil)

(setq mail-default-directory "~/mail/"
      mail-personal-alias-file "~/mail/aliases"
      mail-signature-file ".signature"
      sendmail-program "/usr/bin/msmtp")

(add-hook 'rmail-show-message-hook (lambda () (goto-address-mode 1)))

(defun rmail-archive ()
  (interactive)
  (rmail-output "~/mail/archive.spool"))

(define-key rmail-mode-map (kbd "d") 'rmail-archive)
(define-key rmail-summary-mode-map (kbd "d") 'rmail-archive)
(define-key rmail-mode-map (kbd "D") 'rmail-delete-forward)
(define-key rmail-summary-mode-map (kbd "D") 'rmail-delete-forward)

(defun rmail-view-archive ()
  (interactive)
  (if (or (string= (buffer-name) "inbox.spool")
          (string= (buffer-name) "inbox.spool-summary"))
    (rmail-input "~/mail/archive.spool")
    (rmail-input "~/mail/inbox.spool")))

(define-key rmail-mode-map (kbd "I") 'rmail-view-archive)
(define-key rmail-summary-mode-map (kbd "I") 'rmail-view-archive)
