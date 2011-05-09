(require 'org-install)
(require 'org-publish)

(setq org-publish-project-alist
      '(("org-notes"
         :base-directory "~/pub"
         :base-extension "org"
         :publishing-directory "/tmp/website/"
         :recursive t
         :publishing-function org-publish-org-to-html
         :headline-level 4
         :section-numbers nil
         :table-of-contents nil
         :drawers t
         :creator-info nil
         :link-home "/"
         :style "<link rel='stylesheet' href='/etc/main.css' />")
        ("org-static"
         :base-directory "~/pub"
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
         :publishing-directory "/tmp/website/"
         :publishing-function org-publish-attachment
         :recursive t)
        ("org" :components ("org-notes" "org-static"))))

(defun my-org-export-format-drawer (name content)
  (concat "#+BEGIN_HTML\n"
          "<div class=\"drawer " (downcase name) "\">\n"
          "<h6>" (capitalize name) "</h6>\n"
          "#+END_HTML\n"
          content
          "\n#+HTML: </div>"))
(setq org-export-format-drawer-function 'my-org-export-format-drawer)
