(setq
 rmail-displayed-headers "^To:\\|From:\\|Date:\\|Subject:"
 rmail-file-name "~/mail/inbox"
 rmail-primary-inbox-list '("/var/spool/mail/pavpanchekha" "~/mail/reuse.mit")
 mail-default-directory "~/mail/"
 mail-personal-alias-file "~/mail/aliases"
 mail-signature-file "~/mail/signature"
 sendmail-program "/usr/bin/msmtp")

(add-hook 'rmail-mode-hook (lambda ()
  (define-key rmail-summary-mode-map (kbd "j") 'next-line)
  (define-key rmail-summary-mode-map (kbd "k") 'previous-line)))
