;; hoogle, hasktags, stylish haskell

;; Sometimes all your require is an inferior mode
(use-package haskell-mode
  :ensure t
  :config 
  (progn
    (add-hook 'haskell-mode-hook 'inf-haskell-mode)
    (add-hook 'haskell-mode-hook 'haskell-indent-mode)))

(defun haskell-sibi-lhs-process ()
  (interactive)
  (save-excursion
  (goto-char (point-min))
  (while (re-search-forward "^> " nil t)
    (replace-match ""))
  ))

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
