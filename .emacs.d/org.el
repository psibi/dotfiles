(use-package org
  :bind (("C-c c" . org-capture))
  :custom
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (org-log-done t)
  (org-agenda-files
   '("~/github/timebox/home.org" "~/github/timebox/learn.org" "~/github/timebox/oss.org"))
  (org-default-notes-file "~/github/misc/notes.org")
  (org-capture-templates '(("t" "Personal Task"  entry
                            (file org-default-notes-file)
                            "* TODO %?" :empty-lines 1)
                           ("s" "Standup" entry
                            (file+headline "~/github/timebox/status.org" "Greatcall")
                            "** %k \n %?" :empty-lines 1))))

(use-package org-journal
  :ensure t
  :init
  :custom
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "~/github/timebox/2020/")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-skip-carryover-drawers (list "CLOCKING"))
  (org-journal-file-type 'daily))

(use-package ox-twbs
  :ensure t)
