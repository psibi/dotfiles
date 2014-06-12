;; hoogle, hasktags

(custom-set-variables
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-log t))

;; Make sure that you use latest cabal
(custom-set-variables
  '(haskell-process-type 'cabal-repl))

;; Haskell-mode bindings
(define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
(define-key haskell-mode-map (kbd "C-'") 'haskell-interactive-bring)
(define-key haskell-mode-map (kbd "C-c C-t") 'haskell-process-do-type)
(define-key haskell-mode-map (kbd "C-c C-i") 'haskell-process-do-info)
(define-key haskell-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-mode-map (kbd "C-c c") 'haskell-process-cabal)
(define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)

;; Cabal-mode bindings
(define-key haskell-cabal-mode-map (kbd "C-'") 'haskell-interactive-bring)
(define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
(define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
(define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)

;; Make sure hasktags is installed
(custom-set-variables
 '(haskell-tags-on-save t))

(define-key haskell-mode-map (kbd "M-.") 'haskell-mode-tag-find)

;; Suggestion for removing redundant input lines
(custom-set-variables
 '(haskell-process-suggest-remove-import-lines t))

;; Make sure your hoogle data is generated properly
(custom-set-variables
  '(haskell-process-suggest-hoogle-imports t))

;; Some hooks
(add-hook 'haskell-mode-hook 'turn-on-hi2)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)+ 3
