
(setq org-directory "~/notes"
      org-agenda-files '("~/notes/todo.org")
      org-todo-keywords '(
        (sequence "TODO" "|" "DONE" "WAIT")
        (sequence "SOMEDAY" "|" "DONE")
        (sequence "TODO" "READY" "CODED" "TESTED" "FIXED" "DONE"))
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

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
