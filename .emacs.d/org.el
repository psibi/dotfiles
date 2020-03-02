(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

(setq org-agenda-files
'("~/github/timebox/home.org" "~/github/timebox/learn.org" "~/github/timebox/oss.org"))

(use-package org-journal
  :ensure t
  :init
  :custom
  (org-journal-dir "~/github/timebox/2020/")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-file-type 'daily))