
(setq org-directory "~/notes"
      org-agenda-files '("~/notes/")
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

(setq org-pretty-entities t
      org-entities-user '(("CC" "\\CC" t "&#8450;" "C" "C" "ℂ")
                          ("FF" "\\FF" t "&#120125;" "F" "F" "𝔽")
                          ("HH" "\\HH" t "&#8461;" "H" "H" "ℍ")
                          ("NN" "\\NN" t "&#8469;" "N" "N" "ℕ")
                          ("PP" "\\PP" t "&#8473;" "P" "P" "ℙ")
                          ("QQ" "\\QQ" t "&#8474;" "Q" "Q" "ℚ")
                          ("RR" "\\RR" t "&#8477;" "R" "R" "ℝ")
                          ("ZZ" "\\ZZ" t "&#8484;" "Z" "Z" "ℤ")))

(add-hook 'remember-mode-hook 'org-remember-apply-template)
(define-key global-map [(control meta ?r)] 'remember)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
