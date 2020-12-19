(use-package org
  :ensure t
  :bind (("C-c c" . org-capture))
  :custom
  (org-clock-into-drawer "CLOCKING")
  (define-key global-map "\C-cl" 'org-store-link)
  (define-key global-map "\C-ca" 'org-agenda)
  (org-log-done t)
  (org-agenda-files
   '("/home/sibi/github/timebox/home.org" "/home/sibi/github/timebox/learn.org" "/home/sibi/github/timebox/oss.org" "/home/sibi/github/misc/notes.org" "/home/sibi/github/timebox/parents.org" "/home/sibi/github/timebox/work.org"))
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

;; (use-package ox-twbs
;;   :ensure t)

;; https://orgmode.org/worg/org-contrib/babel/languages/index.html#configure
(org-babel-do-load-languages
 'org-babel-load-languages
 '((awk . t)
   (shell . t)
   (gnuplot . t)))

;; https://emacs.stackexchange.com/questions/23946/how-can-i-stop-the-confirmation-to-evaluate-source-code-when-exporting-to-html
(setq org-confirm-babel-evaluate nil)

(add-to-list 'org-structure-template-alist '("g" . "src sh :exports both :eval never-export :results verbatim\n"))
(add-to-list 'org-structure-template-alist '("p" . "src awk :in-file countries :exports both :results value verbatim\n"))
;; (add-hook 'after-init-hook 'org-agenda-list)
;; (setq initial-buffer-choice '(lambda () (progn (require 'org ) (get-buffer org-agenda-buffer-name))))

;; https://linevi.ch/en/org-pygments.html
(require 'ox)
(require 'ox-html)
(defvar pygments-path "pygmentize")

(defun pygments-org-html-code (code contents info)
  ;; Generating tmp file path.
  ;; Current date and time hash will ideally pass our needs.
  (setq temp-source-file (format "/tmp/pygmentize-%s.txt"(md5 (current-time-string))))
  ;; Writing block contents to the file.
  (with-temp-file temp-source-file (insert (org-element-property :value code)))
  ;; Exectuing the shell-command an reading an output
  (shell-command-to-string (format "%s -l \"%s\" -f html %s"
				   pygments-path
				   (or (org-element-property :language code)
				       "")
				   temp-source-file)))

;; (org-export-define-derived-backend 'org-sibi-html 'twbs
;;   :translate-alist '((src-block .  pygments-org-html-code)
;; 		     (example-block . pygments-org-html-code)))

;; (fset 'org-twbs-src-block 'pygments-org-html-code)

(use-package htmlize
  :ensure t)

(load-file "~/github/ox-twbs/ox-twbs.el")

;; https://emacs.stackexchange.com/questions/44958/can-i-insert-a-prefix-to-org-babel-source-code-lines-on-export
(defun my-insert-shell-prompt (_backend)
  (org-babel-map-src-blocks nil         ; nil implies current buffer
    (let (;; capture macro-defined variables
         (lang lang)
         (beg-body beg-body)
         (end-body end-body)
         ;; other variables
         (shell-langs '("sh" "shell"))
         (prefix "$ "))
      (when (member lang shell-langs)
        (goto-char beg-body)
        (skip-chars-forward "\n\s-" end-body)
        (while (< (point) end-body)
          (insert prefix)
          (end-of-line)
          (skip-chars-forward "\n\s-" end-body))))))

(add-hook 'org-export-before-parsing-hook #'my-insert-shell-prompt)
