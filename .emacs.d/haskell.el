(use-package haskell-mode
  :ensure t
  :init
  :bind (:map haskell-mode-map
              ("C-c C-c C-u" . haskell-compile))
  :config
  (progn
    (add-hook 'haskell-mode-hook 'haskell-indent-mode)
    (add-hook 'literate-haskell-mode-hook 'sibi-literate-haskell-bindings)
    (customize-set-variable 'haskell-hoogle-url '"https://www.stackage.org/lts/hoogle?q=%s")
    (customize-set-variable 'haskell-process-type 'stack-ghci)))



;; (setq package-check-signature nil)

(use-package ormolu
  :ensure t)

(use-package lsp-haskell
  :ensure t
  :custom
  (lsp-haskell-process-path-hie "haskell-language-server-wrapper"))


(use-package hlint-refactor
  :ensure t
  :config
  :hook (haskell-mode . hlint-refactor-mode))
