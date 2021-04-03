;; hoogle, hasktags, stylish haskell

;; Sometimes all your require is an inferior mode
(use-package haskell-mode
  :ensure t
  :init
  :config
  (progn
    (add-hook 'haskell-mode-hook 'haskell-indent-mode)
    (add-hook 'literate-haskell-mode-hook 'sibi-literate-haskell-bindings)
    (customize-set-variable 'haskell-hoogle-url '"https://www.stackage.org/lts/hoogle?q=%s")
    (customize-set-variable 'haskell-process-type 'stack-ghci)))

    ;;


(defun sibi-literate-haskell-bindings ()
  (local-set-key (kbd "C-c >") 'haskell-lhs-codify)
  (local-set-key (kbd "C-c <") 'haskell-lhs-clean))

(defun haskell-lhs-codify (beginning end)
  (interactive "r")
  (if (use-region-p)
      (save-restriction
        (narrow-to-region beginning end)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^" nil t)
            (replace-match "> "))))
    t
    ))

(setq package-check-signature nil)

;; optional if you want which-key integration
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-l")
  :commands lsp
  :hook (haskell-mode . lsp)
  :config)

(use-package lsp-haskell
  :ensure t
  :custom
  (lsp-haskell-process-path-hie "haskell-language-server-wrapper"))

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)

(use-package helm-lsp
  :ensure t
  :commands helm-lsp-workspace-symbol)

(use-package hlint-refactor
  :ensure t
  :config
  :hook (haskell-mode . hlint-refactor-mode))

;; (use-package intero
;;   :ensure t
;;   :init
;;   (add-hook 'haskell-mode-hook 'intero-mode))

;; (use-package hindent
;;   :ensure t
;;   :init
;;   (progn
;;     (flymake-mode -1)                      ; Disable flymake mode
;;     (setq hindent-reformat-buffer-on-save f)
;;     (add-hook 'haskell-mode-hook 'hindent-mode)))

(defun haskell-lhs-clean (beginning end)
  (interactive "r")
  (if (use-region-p)
      (save-restriction
        (narrow-to-region beginning end)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward "^> " nil t)
            (replace-match "    "))
          (while (re-search-forward "^λ>" nil t)
            (replace-match "    λ>")
            (forward-line 1)
            (insert "    "))))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^> *" nil t)
        (replace-match "    "))
      (while (re-search-forward "^λ>" nil t)
        (replace-match "    λ>")
        (forward-line 1)
        (insert "    "))
      )))

;; (custom-set-variables
;;   '(haskell-process-suggest-remove-import-lines t)
;;   '(haskell-process-auto-import-loaded-modules t)
;;   '(haskell-process-log t))

;; ;; Make sure that you use latest cabal
;; (custom-set-variables
;;  '(haskell-process-type 'cabal-repl))

;; (add-hook 'haskell-cabal-mode-hook 'haskell-cabal-hook)
;; ;; Haskell
;; (add-hook 'haskell-mode-hook 'turn-on-hi2)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

;; ;; Stylish haskell
;; (custom-set-variables
;;  '(haskell-stylish-on-save t))

;; ;; Haskell-mode bindings
;; (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;; (define-key haskell-mode-map (kbd "C-'") 'haskell-interactive-bring)
;; (define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
;; (define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
;; (define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;; (define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;; (define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
;; (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

;; ;; Useful to have these keybindings for .cabal files, too.
;; (defun haskell-cabal-hook ()
;;   (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;;   (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)
;;   (define-key haskell-cabal-mode-map (kbd "C-`") 'haskell-interactive-bring)
;;   (define-key haskell-cabal-mode-map [?\C-c ?\C-z] 'haskell-interactive-switch))

;; ;; Make sure hasktags is installed
;; (custom-set-variables
;;  '(haskell-tags-on-save t))

;; (define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

;; ;; Suggestion for removing redundant input lines
;; (custom-set-variables
;;  '(haskell-process-suggest-remove-import-lines t))

;; ;; Make sure your hoogle data is generated properly
;; (custom-set-variables
;;   '(haskell-process-suggest-hoogle-imports t))

;; ;; Some hooks
;; (add-hook 'haskell-mode-hook 'turn-on-hi2)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
;; (add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)

(setq flymake-run-in-place nil)
(setq temporary-file-directory "~/.emacs.d/tmp/")
