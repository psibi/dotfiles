(use-package ormolu
  :ensure t
  :init)

(use-package haskell-mode
  :ensure t
  :init
  :after (lsp-mode)
  :bind (:map haskell-mode-map
              ("C-c C-c <tab>" . haskell-compile)
              ("C-c C-c C-f" . ormolu-format-buffer)
              ("C-c C-c C-i" . haskell-stack-install)
              ("C-c C-c C-t" . haskell-stack-test))
  :custom
  (haskell-mode-hook '(haskell-indentation-mode lsp-deferred))
  (haskell-compiler-type 'stack)
  (haskell-process-type 'stack-ghci)
  (haskell-hoogle-url '"https://www.stackage.org/lts/hoogle?q=%s"))

(use-package lsp-haskell
  :ensure t
  :after (haskell-mode)
  :custom
  (lsp-haskell-process-path-hie "haskell-language-server-wrapper"))

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
