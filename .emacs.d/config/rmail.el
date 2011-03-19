(custom-set-variables
 '(rmail-file-name "~/mail/inbox")
 '(rmail-default-body-file "~/mail/out")
 '(rmail-default-file "~/mail")
 '(rmail-primary-inbox-list (quote ("/var/spool/mail/pavpanchekha" "~/mail/reuse.mit" "~/mail/remind-fans" "lisp-hug")))
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
