
(add-hook 'c-mode-common-hook
          '(lambda ()
             (turn-on-auto-fill)                ; Automatically break at column 80 or something
             (setq fill-column 80)              ; Column 80 for code
             (setq comment-column 60)           ; Column 60 for comments
             
             (setq c-hungry-delete-key t)       ; C-c C-d deletes a whole block of whitespace
             (setq c-auto-newline 1)            ; Certain characters automatically start a new line
             (modify-syntax-entry ?_ "w")       ; now '_' is not considered a word-delimiter
             (c-set-style "ellemtel")           ; set indentation style
             (local-set-key [(control tab)]     ; move to next tempo mark
                            'tempo-forward-mark)))
