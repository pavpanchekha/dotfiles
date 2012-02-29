(setq org-export-latex-listings t)

(defun latex-class (name header)
  (list name header
        '("\\section{%s}" . "\\section*{%s}")
        '("\\subsection{%s}" . "\\subsection*{%s}")
        '("\\subsubsection{%s}" . "\\subsubsection*{%s}")
        '("\\paragraph{%s}" . "\\paragraph*{%s}")
        '("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

(setq org-export-latex-classes
      (list (latex-class "pset"    "\\documentclass{pset}")
            (latex-class "simple"  "\\documentclass{simple}")
            (latex-class "article" "\\documentclass{article}")))
