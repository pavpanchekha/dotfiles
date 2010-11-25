
(custom-set-variables
 '(jabber-chat-buffer-show-avatar nil)
 '(jabber-chat-foreign-prompt-format "%t] ")
 '(jabber-chat-header-line-format (quote (" " (:eval (jabber-jid-displayname jabber-chatting-with)) " " (:eval (let ((buddy (jabber-jid-symbol jabber-chatting-with))) (propertize (or (cdr (assoc (get buddy (quote show)) jabber-presence-strings)) (get buddy (quote show))) (quote face) (or (cdr (assoc (get buddy (quote show)) jabber-presence-faces)) (quote jabber-roster-user-online))))) " " jabber-chatstates-message)))
 '(jabber-chat-local-prompt-format "%t] ")
 '(jabber-chat-system-prompt-format "%t] *** ")
 '(jabber-groupchat-prompt-format "%t] %n> ")
 '(jabber-roster-line-format " %c %-20n")
 '(jabber-roster-show-title nil)
 '(jabber-show-offline-contacts nil)
 '(jabber-show-resources nil)
 '(jabber-sort-order (quote ("chat" "" "dnd" "away" "xa")))
 '(jabber-vcard-avatars-retrieve nil))

(custom-set-faces
 '(jabber-chat-prompt-foreign ((t (:foreground "coral"))))
 '(jabber-chat-prompt-local ((t (:foreground "sky blue"))))
 '(jabber-rare-time-face ((t (:foreground "light green" :underline t))))
 '(jabber-roster-user-away ((t (:foreground "gold" :slant italic :weight normal))))
 '(jabber-roster-user-dnd ((t (:foreground "coral" :slant italic :weight normal))))
 '(jabber-roster-user-online ((t (:foreground "pale green" :slant normal :weight semi-light))))
 '(jabber-title-large ((t (:inherit variable-pitch :weight bold :height 2.0 :width ultra-expanded))))
 '(jabber-title-medium ((t (:inherit variable-pitch :weight bold :height 1.4 :width expanded)))))

(setq jabber-account-list
      `(("pavpanchekha@gmail.com"
         (:network-server . "talk.google.com")
         (:connection-type . ssl)
         (:password . ,(keyring-get-password "Jabber" "pavpanchekha")))))

; The jabber window goes into its own frame
(setq 
  special-display-regexps 
  '(("*-jabber-roster-*" 
      (width . 34)
      (tool-bar-lines . 0)
      (menu-bar-lines 0)
      (left . 80))))

; Auto-urlize urls
(add-hook 'jabber-chat-mode-hook 'goto-address)
(add-hook 'jabber-chat-mode-hook (lambda () (setq fill-column 34)))
(jabber-connect-all)

