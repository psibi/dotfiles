(use-package org
  :ensure t
  :bind (("C-c c" . org-capture))
  :custom
  (org-clock-into-drawer "CLOCKING")
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (org-confirm-babel-evaluate nil)
  (org-export-babel-evaluate nil)
  (org-log-done t)
  ;; (initial-buffer-choice org-agenda-buffer-name)
  :config
  (add-hook 'after-init-hook 'org-agenda-list))


(use-package sage-shell-mode
  :ensure t
  :custom
  (sage-shell:use-simple-prompt t))

(use-package ob-sagemath
  :ensure t
  :custom
  (org-babel-default-header-args:sage '((:session . t)
                                        (:results . "output"))))

(use-package org-journal
  :ensure t
  :custom
  (org-journal-file-format "%Y-%m-%d.org")
  (org-journal-dir "/home/sibi/github/timebox/2025/")
  (org-journal-date-format "%A, %d %B %Y")
  (org-journal-skip-carryover-drawers (list "CLOCKING"))
  (org-journal-file-type 'daily))

(use-package org-roam
  :ensure t
  :bind (("C-c C-g" . org-roam-node-find))
  :custom
  (org-roam-directory "/home/sibi/github/misc/roam"))
;; If facing DB issues, use org-roam-db-sync

;; (use-package htmlize
;;   :ensure t)

;; (straight-use-package
;;   '(el-patch :type git :host github :repo "psibi/ox-twbs"))

;; (load-file "~/github/ox-twbs/ox-twbs.el")

;; https://emacs.stackexchange.com/questions/44958/can-i-insert-a-prefix-to-org-babel-source-code-lines-on-export
;; (defun my-insert-shell-prompt (_backend)
;;   (org-babel-map-src-blocks nil         ; nil implies current buffer
;;     (let (;; capture macro-defined variables
;;          (lang lang)
;;          (beg-body beg-body)
;;          (end-body end-body)
;;          ;; other variables
;;          (shell-langs '("sh" "shell"))
;;          (prefix "$ "))
;;       (when (member lang shell-langs)
;;         (goto-char beg-body)
;;         (skip-chars-forward "\n\s-" end-body)
;;         (while (< (point) end-body)
;;           (insert prefix)
;;           (end-of-line)
;;           (skip-chars-forward "\n\s-" end-body))))))

;; (add-hook 'org-export-before-parsing-hook #'my-insert-shell-prompt)

;; (use-package ob-http
;;   :ensure t)

;; (use-package ob-typescript
;;   :ensure t)


;; https://orgmode.org/worg/org-contrib/babel/languages/index.html#configure
(org-babel-do-load-languages
 'org-babel-load-languages
 '((awk . t)
   (shell . t)
   ;; (http . t)
   (octave . t)
   (haskell . t)
   (gnuplot . t)
   (js . t)
   (sql . t)))

;; https://emacs.stackexchange.com/questions/23946/how-can-i-stop-the-confirmation-to-evaluate-source-code-when-exporting-to-html
(setq org-confirm-babel-evaluate nil)

(add-to-list 'org-structure-template-alist '("g" . "src sh :exports both :eval never-export :results verbatim\n"))
(add-to-list 'org-structure-template-alist '("p" . "src sage :session nix :eval never-export :results value verbatim output :exports both\n"))
