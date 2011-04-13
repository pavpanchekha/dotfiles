(require 'gnus)
(setq nnrss-directory "~/mail/rss"
      gnus-inhibit-startup-message t
      gnus-init-file "~/.emacs.d/gnus/startup.el"
      gnus-startup-file "~/.emacs.d/gnus/newsrc"
      message-default-charset `utf-8
      gnus-select-methods '((nnrss "news"))
      gnus-secondary-select-methods '((nnmbox "email"))
      mail-source-directory "~/mail/"
      mail-sources '((directory :path "~/mail" :prescript "fetchmail")))
      
(setq mail-default-directory "~/mail/"
      mail-personal-alias-file "~/mail/aliases"
      mail-signature-file "~/mail/signature"
      sendmail-program "/usr/bin/msmtp")

(setq nnmail-split-methods
      '(("Other" "")))

(add-hook 'gnus-article-display-hook
          '(lambda ()
	       (gnus-article-de-quoted-unreadable)
	       (gnus-article-emphasize)
	       (gnus-article-hide-boring-headers)
	       (gnus-article-hide-headers-if-wanted)
	       (gnus-article-hide-pgp)
	       (gnus-article-highlight)
	       (gnus-article-highlight-citation)
	       ))

(setq gnus-message-archive-method
	'(nnfolder "archive"
		   (nnfolder-inhibit-expiry t)
		   (nnfolder-active-file "~/mail/sent-mail/active")
		   (nnfolder-directory "~/mail/sent-mail/")
		   (nnfolder-get-new-mail nil)))
