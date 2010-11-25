
(setq org-directory "~/notes"
      org-agenda-files '("~/notes/todo.org")
      org-todo-keywords '((sequence "SOMEDAY(s)" "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "CANCELED(c@)"))
      org-default-notes-files "~/notes.notes.org"
      org-agenda-ndays 7
      org-agenda-show-all-dates t
      org-agenda-skip-deadline-if-done t
      org-agenda-skip-scheduled-if-done t
      org-agenda-start-on-weekday nil
      org-reverse-note-order t
      org-remember-templates '((116 "* TODO %?\n  %u" "~/todo.org" "Tasks") (110 "* %u %?" "~/notes.org" "Notes")))

(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map [(control meta ?r)] 'remember)
