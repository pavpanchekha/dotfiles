(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-mode t)
 '(indicate-empty-lines t)
 '(inhibit-startup-screen t)
 '(menu-bar-mode t)
 '(org-capture-templates (quote (("e" "Event" entry (file "~/notes/events.org") "* %^{Description} %^t" :prepend t :immediate-finish t) ("t" "Todo Entry" entry (file "~/notes/pavel.org") "TODO %^{Description}" :prepend t :immediate-finish t))))
 '(org-export-creator-info nil)
 '(org-export-mark-todo-in-toc t)
 '(org-export-run-in-background t)
 '(org-export-with-section-numbers nil)
 '(org-export-with-toc 1)
 '(message-directory "~/mail")
 '(preview-scale-function 1.1)
 '(scroll-bar-mode nil)
 '(shell-prompt-pattern "\\.\\*:%> ")
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(tramp-default-user nil)
 '(tramp-default-user-alist (quote (("synce" nil nil) ("imaps" nil "pavpanchekha") ("imap" nil "pavpanchekha") ("socks" nil "pavpanchekha") ("tunnel" nil "pavpanchekha") ("fish" nil "pavpanchekha") ("smb" nil "") ("\\`su\\(do\\)?\\'" nil "pavpanchekha") ("\\`r\\(em\\)?\\(cp\\|sh\\)\\|telnet\\|plink1?\\'" nil "pavpanchekha"))))
 '(w3m-arrived-file "/tmp/w3m-pavpanchekha/arrived")
 '(w3m-bookmark-file-coding-system (quote utf-8))
 '(w3m-cookie-file "/tmp/w3m-pavpanchekha/cookie")
 '(w3m-default-directory "~/")
 '(w3m-form-textarea-directory "/tmp/w3m-pavpanchekha/textarea")
 '(w3m-form-textarea-edit-mode (quote org-mode))
 '(w3m-make-new-session t)
 '(w3m-profile-directory "/tmp/w3m-pavpanchekha/w3m")
 '(w3m-session-autosave t)
 '(w3m-session-crash-recovery nil)
 '(w3m-session-file "/tmp/w3m-pavpanchekha/sessions"))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(jabber-chat-prompt-foreign ((t (:foreground "red"))))
 '(jabber-chat-prompt-local ((t (:foreground "blue"))))
 '(jabber-chat-prompt-system ((t (:foreground "dark green" :weight bold))))
 '(jabber-roster-user-away ((t (:foreground "orange"))))
 '(jabber-roster-user-chatty ((t (:foreground "green"))))
 '(jabber-roster-user-online ((t (:foreground "dark green")))))
