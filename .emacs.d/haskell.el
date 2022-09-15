(use-package ormolu
  :ensure t
  :init)

(use-package haskell-mode
  :ensure t
  :init
  :bind (:map haskell-mode-map
              ("C-c C-c <tab>" . haskell-compile)
              ("C-c C-c C-f" . ormolu-format-buffer)
              ("C-c C-c C-i" . haskell-stack-install)
              ("C-c C-c C-t" . haskell-stack-test))
  :config
  (progn
    (add-hook 'haskell-mode-hook 'haskell-indent-mode)
    (customize-set-variable 'haskell-process-type 'stack-ghci)
    (customize-set-variable 'haskell-hoogle-url '"https://www.stackage.org/lts/hoogle?q=%s")))

(use-package lsp-haskell
  :ensure t
  :custom
  (lsp-haskell-process-path-hie "haskell-language-server-wrapper")
  :hook (haskell-mode . lsp-deferred))

;; (use-package hlint-refactor
;;   :ensure t
;;   :config
;;   :hook (haskell-mode . hlint-refactor-mode))

(defun haskell-stack-install ()
  (interactive)
  (let ((haskell-compile-stack-build-command "stack install --fast"))
    (haskell-compile)))

(defun haskell-stack-test ()
  (interactive)
  (let ((haskell-compile-stack-build-command "stack test --fast"))
    (haskell-compile)))
