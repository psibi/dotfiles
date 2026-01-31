(use-package haskell-mode
  :ensure t
  :after (lsp-mode)
  :bind (:map haskell-mode-map
              ("C-c C-c <tab>" . haskell-compile)
              ("C-c C-c C-f" . ormolu-format-buffer)
              ("C-c C-c C-i" . haskell-stack-install)
              ("C-c C-c C-t" . haskell-cabal-test))
  :custom
  (haskell-mode-hook '(haskell-indentation-mode lsp-deferred))
  (haskell-compiler-type 'stack)
  (haskell-process-type 'stack-ghci)
  (lsp-rename-use-prepare nil)
  (lsp-haskell-formatting-provider "fourmolu")
  (haskell-hoogle-url '"https://www.stackage.org/lts/hoogle?q=%s"))

(use-package lsp-haskell
  :ensure t
  ;; :quelpa (lsp-haskell :fetcher file
  ;;                   :path "~/github/lsp-haskell/"
  ;;                   :files ("*.el"))
  :after (haskell-mode)
  :custom
  ;; https://github.com/emacs-lsp/lsp-mode/issues/4473#issuecomment-2308978908
  (lsp-rename-use-prepare nil)
  (lsp-haskell-process-path-hie "haskell-language-server-wrapper")
  (lsp-haskell-formatting-provider "fourmolu")
  (lsp-haskell-plugin-tactics-global-on t)
  (lsp-haskell-plugin-import-lens-code-lens-on t)
  (lsp-haskell-plugin-class-global-on t)
  (lsp-haskell-plugin-eval-global-on t)
  (lsp-haskell-plugin-retrie-global-on t)
  (lsp-haskell-plugin-splice-global-on t)
  (lsp-haskell-plugin-module-name-global-on t)
  (lsp-haskell-plugin-ghcide-completions-config-snippets-on t)
  (lsp-haskell-plugin-ghcide-type-lenses-global-on t)
  (lsp-haskell-plugin-ghcide-completions-config-auto-extend-on t)
  (lsp-haskell-plugin-hlint-diagnostics-on t)
  (lsp-haskell-plugin-hlint-code-actions-on t)
  (lsp-haskell-plugin-rename-config-cross-module t)
  (lsp-haskell-plugin-import-lens-code-actions-on t)
  (lsp-haskell-plugin-haddock-comments-global-on t)
  (lsp-haskell-plugin-pragmas-code-actions-on t)
  (lsp-haskell-plugin-pragmas-completion-on t)
  (lsp-haskell-plugin-refine-imports-global-on t))

(defun haskell-stack-install ()
  "Stack install your project."
  (interactive)
  (let ((haskell-compile-stack-build-command "stack install --fast"))
    (haskell-compile)))

(defun haskell-nix-build ()
  "Nix build your project."
  (interactive)
  (let ((haskell-compile-cabal-build-command "ghci <<< ':serve'"))
    (haskell-compile)))

(defun haskell-nix-test ()
  "Nix test your project."
  (interactive)
  (let ((haskell-compile-cabal-build-command "ghci <<< ':test'"))
    (haskell-compile)))

(defun haskell-stack-test ()
  "Stack test your project."
  (interactive)
  (let ((haskell-compile-stack-build-command "stack test --fast"))
    (haskell-compile)))

(defun haskell-cabal-test ()
  "Cabal test your project."
  (interactive)
  (let ((haskell-compile-cabal-build-command "cabal test"))
    (haskell-compile)))
