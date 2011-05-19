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
 '(preview-scale-function 1.1)
 '(scroll-bar-mode nil)
 '(size-indication-mode t)
 '(tool-bar-mode nil))
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
