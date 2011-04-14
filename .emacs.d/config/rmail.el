(require 'rmail-extras)

(custom-set-variables
 '(rmail-default-body-file "/tmp/pavpanchekha-compose")
 '(rmail-default-file "~/mail")
 '(rmail-file-name "~/mail/inbox.spool")
 '(rmail-preserve-inbox t)
 '(rmail-inbox-list '("~/mail/inbox.spool" "~/mail/reuse.mit.spool" "~/mail/remind-fans.spool" "~/mail/lisp-hug.spool"))
 '(rmail-redisplay-summary t)
 '(rmail-secondary-file-directory "~/mail")
 '(rmail-secondary-file-regexp "~/mail/*")

 '(rmail-mime-show-images nil)
 '(rmail-displayed-headers "^To:\\|From:\\|Date:\\|Subject:")

 '(rmail-display-summary t)
 '(rmail-summary-scroll-between-messages nil))

(setq
 mail-default-directory "~/mail/"
 mail-personal-alias-file "~/mail/aliases"
 mail-signature-file "~/mail/signature"
 sendmail-program "/usr/bin/msmtp")

(add-hook 'rmail-mode-hook (lambda ()
  (define-key rmail-summary-mode-map (kbd "j") 'next-line)
  (define-key rmail-summary-mode-map (kbd "k") 'previous-line)))
