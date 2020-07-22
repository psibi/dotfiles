(use-package org
  :ensure t
  :bind (("C-c c" . org-capture))
  :custom
  (org-clock-into-drawer "CLOCKING")
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (org-log-done t)
  (org-agenda-files
   '("/home/sibi/github/timebox/home.org" "/home/sibi/github/timebox/learn.org" "/home/sibi/github/timebox/oss.org"))
  (org-default-notes-file "/home/sibi/github/misc/notes.org")
  (org-capture-templates '(("t" "Personal Task"  entry
                            (file org-default-notes-file)
                            "* TODO %?" :empty-lines 1)
                           ("s" "Open Source task" entry
                            (file+headline "/home/sibi/github/timebox/oss.org" "Others")
                            "** TODO %?" :empty-lines 1)))
  (initial-buffer-choice '(get-buffer org-agenda-buffer-name))
  :config
  (add-hook 'after-init-hook 'org-agenda-list))

(use-package org-journal
  :ensure t
  :custom
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "/home/sibi/github/timebox/2020/")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-skip-carryover-drawers (list "CLOCKING"))
  (org-journal-file-type 'daily))

(use-package org-roam
  :ensure t
  :custom
  (org-roam-directory "/home/sibi/github/misc/roam")
  :config
  (add-hook 'after-init-hook 'org-roam-mode))

(use-package ox-twbs
  :ensure t)

;; (add-hook 'after-init-hook 'org-agenda-list)
;; (setq initial-buffer-choice '(lambda () (progn (require 'org ) (get-buffer org-agenda-buffer-name))))
