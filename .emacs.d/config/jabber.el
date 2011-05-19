(require 'jabber)

; Some variables
(setq jabber-account-list '(("pavpanchekha@gmail.com" 
                             (:network-server . "talk.google.com") 
                             (:connection-type . ssl)))
      jabber-avatar-cache-directory "/tmp/jabber-avatars"
      jabber-backlog-days 3.0
      jabber-chat-buffer-show-avatar nil
      jabber-chat-foreign-prompt-format "> "
      jabber-chat-local-prompt-format "> "
      jabber-chat-system-prompt-format "*** "
      jabber-chat-time-format "%H:%M"
      jabber-default-show ""
      jabber-groupchat-prompt-format "%n> "
      jabber-muc-private-foreign-prompt-format "%g/%n> "
      jabber-roster-line-format "%c %-25n %u %-8s  %S"
      jabber-roster-show-title nil
      jabber-show-offline-contacts nil)

(custom-set-faces
 '(jabber-chat-prompt-foreign ((t (:foreground "red"))))
 '(jabber-chat-prompt-local ((t (:foreground "blue"))))
 '(jabber-chat-prompt-system ((t (:foreground "dark green" :weight bold))))
 '(jabber-roster-user-away ((t (:foreground "orange"))))
 '(jabber-roster-user-chatty ((t (:foreground "green"))))
 '(jabber-roster-user-online ((t (:foreground "dark green")))))

; Auto-urlize urls
(add-hook 'jabber-chat-mode-hook 'goto-address)
(jabber-connect-all)

