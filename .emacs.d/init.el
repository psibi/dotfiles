;; My Emacs configuration
;; Author: Sibi <sibi@psibi.in>
;; File path: ~/.emacs.d/init.el

(require 'use-package)

;;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq user-mail-address "sibi@psibi.in")
(setq user-full-name "Sibi Prabakaran")

(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)

;; Save history of minibuffer
(savehist-mode)

;; Make right-click do something sensible
(when (display-graphic-p)
  (context-menu-mode))

;; Unbind C-z
(when window-system
  (global-unset-key [(control z)]))

(if (eq system-type 'darwin)
    (progn
      (set-face-attribute 'default nil :height 150)))

;; https://emacs.stackexchange.com/questions/62049/override-the-default-font-for-emoji-characters
(setq use-default-font-for-symbols nil)
(set-fontset-font t 'symbol "Twitter Color Emoji")

(fset 'yes-or-no-p 'y-or-n-p)
(setq inhibit-startup-message t)

;;; Workaround to make GPG agent work well with ssh keys till I move
;;; managing my xmonad configuration with home manager itself

;; https://github.com/nix-community/home-manager/issues/307
;; https://nix-community.github.io/home-manager/index.html#sec-install-standalone
;; https://github.com/nix-community/home-manager/issues/292#issuecomment-403104476
;; We do this to setup GPG integration properly
(setenv "SSH_AUTH_SOCK"
        (string-trim (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")))

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Remove menu, tool and scroll bar.
(use-package menu-bar
  :ensure nil
  :config
  (menu-bar-mode -1))

(use-package tool-bar
  :ensure nil
  :config
  (tool-bar-mode -1))

(use-package package
  :ensure nil
  :config
  (package-initialize)
  :custom
  (package-native-compile t)
  (package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                      ("melpa" . "https://melpa.org/packages/"))))

(use-package quelpa
  :ensure t
  :custom
  (quelpa-git-clone-depth 1)
  (quelpa-self-upgrade-p nil)
  (quelpa-update-melpa-p nil)
  (quelpa-checkout-melpa-p nil))

(use-package quelpa-use-package
  :ensure t)

(use-package dash
  :ensure t)

(use-package all-the-icons
  :ensure nil)

(use-package doom-themes
  :ensure t
  :after (all-the-icons)
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package doom-modeline
  :ensure t
  :after (all-the-icons)
  :hook (after-init . doom-modeline-mode))

(use-package treemacs
  :ensure t
  :after (all-the-icons)
  :bind
  (:map global-map
        ("C-x t o" . treemacs-select-window)))

(use-package treemacs-projectile
  :after (treemacs projectile)
  :ensure t)

(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode)
  :bind (:map flycheck-mode-map
              ("C-c f l" . flycheck-list-errors)
              ("C-c f n" . flycheck-next-error)
              ("C-c f p" . flycheck-previous-error))
  :custom
  (flycheck-idle-change-delay 0.5)
  (flycheck-sh-shellcheck-executable "shellcheck")
  (flycheck-check-syntax-automatically '(save idle-change mode-enabled)))

(use-package markdown-mode
  :ensure t
  :after (lsp-mode)
  :hook ((markdown-mode . lsp-deferred))
  :bind (:map markdown-mode-map
              ;; ("C-c p" . markdown-previous-visible-heading)
              ("C-c n" . markdown-next-visible-heading))
  :custom
  (markdown-hide-urls t))

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package lsp-mode
  ;; :ensure t
  :quelpa (lsp-mode :fetcher file
                    :path "~/github/lsp-mode"
                    :files ("*.el" "clients/*.el"))
  :init (setq lsp-keymap-prefix "C-l")
  :after (dash)
  :commands lsp
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-configure . lsp-lens-mode))
  :config
  ;; NixOS result symbolic directory
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\result\\'" t)
  :custom
  ;; (lsp-log-io t)
  (lsp-log-io nil)
  (lsp-disabled-clients '(tfls clangd rls rnix-lsp semgrep-ls deno-ls))
  (lsp-semantic-tokens-enable t)
  (lsp-lens-auto-enable t)
  (lsp-semantic-tokens-honor-refresh-requests nil)
  (lsp-semantic-tokens-allow-delta-requests t)
  (lsp-semantic-tokens-allow-ranged-requests t)
  (lsp-semantic-tokens-warn-on-missing-face nil)
  (lsp-diagnostics-provider :flycheck)
  ;; https://github.com/emacs-lsp/lsp-mode/issues/4437#issuecomment-2075100304
  (lsp-eldoc-render-all nil)
  (lsp-inlay-hint-enable t))

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-include-signature t))

(use-package lsp-origami
  :ensure t
  :after (lsp-mode)
  :config
  (add-hook 'lsp-after-open-hook #'lsp-origami-try-enable))

(use-package lsp-markdown
  :ensure lsp-mode
  :after lsp-mode
  :demand t)

(use-package lsp-terraform
  :ensure lsp-mode
  :after lsp-mode
  :demand t
  :custom
  (lsp-terraform-ls-enable-show-reference t)
  (lsp-terraform-ls-prefer-treemacs-all-the-icons-theme t)
  (lsp-terraform-ls-validate-on-save t)
  (lsp-terraform-ls-prefill-required-fields t))

(use-package lsp-rust
  :ensure lsp-mode
  :after lsp-mode
  :demand t
  :custom
  (lsp-rust-analyzer-server-command '("rustup" "run" "stable" "rust-analyzer"))
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-experimental-proc-attr-macros t)
  (lsp-rust-analyzer-proc-macro-enable t)
  (lsp-rust-analyzer-binding-mode-hints nil)
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-rust-analyzer-max-inlay-hint-length 25)
  (lsp-rust-analyzer-closure-capture-hints t))

(use-package lsp-pylsp
  :ensure lsp-mode
  :after lsp-mode
  :demand t
  :hook (python-ts-mode . lsp-deferred))


(use-package ccls
  :ensure t
  :after (lsp-mode)
  :hook ((c-mode c++-mode) . (lambda () (require 'ccls) (lsp))))

(add-to-list 'auto-mode-alist '("\\.ya?ml\\'" . yaml-ts-mode))
(add-hook 'yaml-ts-mode 'lsp-deferred)

;; Debug: For some reason it's not working
;; (use-package yaml-ts-mode
;;   :ensure nil
;;   :after (lsp-mode)
;;   :hook (yaml-ts-mode . lsp-deferred))

(use-package treemacs-all-the-icons
  :ensure t)

(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp-mode treemacs-all-the-icons))

(use-package terraform-mode
  :ensure t
  :after (lsp-mode)
  :hook (terraform-mode . lsp-deferred)
  :bind (:map terraform-mode-map
              ("C-c C-c C-f" . terraform-format-buffer)))

(use-package terraform-doc
  :ensure t)

(use-package sh-script
 :ensure nil
 :mode ("\\.sh\\'" . bash-ts-mode))

(use-package lsp-bash
  :ensure lsp-mode
  :after lsp-mode
  :demand t
  :hook (bash-ts-mode . lsp-deferred))

(use-package rust-mode
  :quelpa (rust-mode :fetcher file
                    :path "~/github/rust-mode")
  :defer t
  :init
  (setq rust-mode-treesitter-derive t))

(use-package rustic
  :quelpa (rustic :fetcher file
                  :path "~/github/rustic")
  ;; :ensure t
  :after (rust-mode lsp-mode smartparens)
  :init
  (progn
    (add-hook 'rustic-mode-hook #'turn-on-smartparens-mode))
  ;; :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ([remap rustic-cargo-init] . rustic-cargo-install)
              ("C-c C-c C-i" . rustic-cargo-install)
              ("C-c C-c <tab>" . rustic-cargo-build)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c o" . lsp-rust-analyzer-open-external-docs)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  (setq rustic-format-on-save nil)
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer"))
  (rustic-default-clippy-arguments nil)
  (rustic-cargo-use-last-stored-arguments t)
  (rustic-babel-auto-wrap-main nil)
  (rustic-babel-display-error-popup nil)
  (rustic-cargo-populate-package-name t)
  (rustic-rustfmt-args "--all")
  (rustic-cargo-default-install-arguments '("--path" "." "--locked" "--offline" "--profile" "dev")))

(use-package yasnippet
  :ensure t
  :after (lsp-mode)
  :hook ((lsp-mode . yas-minor-mode)))

(use-package yasnippet-snippets
  :ensure t
  :bind (("C-c C-y i" . yas-insert-snippet)))

(use-package paredit
  :diminish paredit-mode
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package smartparens
  :ensure t)

(use-package google-this
  :ensure t)

(use-package simple
  :ensure nil
  :config
  (size-indication-mode 1)
  :bind
  (("C-w" . backward-kill-word)
   ("C-x C-k" . kill-region)
   ("C-c C-k" . kill-region))
  :custom
  (next-line-add-newlines nil))

(use-package scroll-bar
  :ensure nil
  :config
  (set-scroll-bar-mode 'nil))

(use-package vc-hooks
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  ;;; https://www.reddit.com/r/emacs/comments/y92y4b/tramp_users_slowness_got_you_down_check/
  (vc-handled-backends '(Git))
  :config
  ;; https://www.reddit.com/r/emacs/comments/4c0mi3/the_biggest_performance_improvement_to_emacs_ive/
  (remove-hook 'find-file-hooks 'vc-find-file-hook))

(use-package files
  :ensure nil
  :custom
  (require-final-newline t)
  (confirm-nonexistent-file-or-buffer nil)
  (backup-directory-alist '(("." . "~/.emacs.d/emacs_backup")))
  (backup-by-copying t)
  (version-control t)
  (kept-old-versions 2)
  (kept-new-versions 20)
  (delete-old-versions t)
  (auto-save-file-name-transforms nil)
  :config
  (add-hook 'before-save-hook 'delete-trailing-whitespace))

(use-package mule
  :ensure nil
  :config
  (prefer-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8))

(use-package tramp
  :ensure nil
  :custom
  ;;Tramp for editing protected files in existing Emacs session.(C-x C-f /sudo)
  (tramp-default-method "ssh")
  (tramp-backup-directory-alist backup-directory-alist))

(use-package ehsell
  :ensure nil
  :bind
  (("C-x m" . eshell)))

(use-package flyspell
  :ensure nil
  :custom
  (ispell-program-name "aspell"))

(use-package octave
  :ensure t)

(use-package magit
  :ensure t
  :init
  :bind (("C-c g" . 'magit-status))
  :custom
  (magit-auto-revert-mode 1)
  (magit-commit-arguments (quote ("--gpg-sign=BB557613")))
  (magit-refresh-status-buffer nil)
  :config
  ;; https://magit.vc/manual/magit/Performance.html
  (progn
    (remove-hook 'server-switch-hook 'magit-commit-hook)
    (remove-hook 'with-editor-filter-visit-hook 'magit-commit-hook)))


;;Projectile related config
(use-package projectile
  :ensure t
  :diminish projectile-mode "p"
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-global-mode)
  :custom
  (projectile-enable-caching t))

;;Helm related config
(use-package helm
  :ensure t
  :diminish helm-mode "h"
  :init
  (helm-mode 1)
  :bind
  (("C-c h" . helm-command-prefix)
   ("M-x" . helm-M-x)
   ("C-x C-m" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-mini)
   ("C-x C-f" . helm-find-files)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . helm-select-action))
  :config
  (progn
    (global-unset-key (kbd "C-x c"))
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))
    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t
          helm-ff-newfile-prompt-p nil)))

(use-package helm-projectile
  :ensure t
  :init
  (progn
    (helm-projectile-on))
  :custom
  (projectile-completion-system 'helm))

(use-package helm-swoop
  :ensure t
  :bind
  (("M-i" . helm-swoop)
   ("M-I" . helm-swoop-back-to-last-point)
   ("C-c M-i" . helm-multi-swoop)
   ("C-x M-i" . helm-multi-swoop-all)
   :map isearch-mode-map
   ("M-i" . helm-swoop-from-isearch)
   :map helm-swoop-map
   ("M-i" . helm-multi-swoop-all-from-helm-swoop))
  :custom
  (helm-multi-swoop-edit-save t)
  (helm-swoop-split-with-multiple-windows nil)
  (helm-swoop-split-direction 'split-window-vertically)
  (helm-swoop-speed-or-color nil)
  (helm-swoop-move-to-line-cycle t)
  (helm-swoop-use-line-number-face t))

(use-package doc-view
  :ensure t
  :config
  (progn
    (add-hook 'doc-view-mode-hook 'auto-revert-mode)))

(use-package ace-window
  :ensure t
  :bind
  (("C-x o" . ace-window)))

(use-package select
  :ensure nil
  :custom
  (x-select-enable-clipboard t))

(use-package dired
  :custom
  (dired-listing-switches "-alh"))

(use-package window
  :ensure nil
  :bind
  (("C-x 2" . split-window-vertically)
   ("C-x 3" . split-window-horizontally)))

(use-package tex-mode
  :ensure t
  :defer t)

(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :custom
  (guide-key/guide-key-sequence '("C-x 4" "C-c p"))
  :config
  (guide-key-mode 1))

(use-package ace-window
  :ensure t)

(use-package ace-jump-mode
  :ensure t
  :bind
  ;; Also with universal argument: C-u C-c SPC
  ;; For line jumping: C-u C-u C-c SPC
  (("C-c SPC" . ace-jump-mode)))

;; (use-package nyan-mode
;;   :ensure t
;;   :config
;;   (nyan-mode 1))

(use-package flycheck-pos-tip
  :ensure t
  :after (flycheck)
  :config
  (flycheck-pos-tip-mode))

(use-package expand-region
  :ensure t)

(use-package centered-cursor-mode
  :ensure t
  :config
  (global-centered-cursor-mode))

(use-package tldr
  :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package isearch
  :ensure nil
  :custom
  (search-whitespace-regexp ".*?"))

(use-package frame
  :ensure nil
  :config
  (toggle-frame-fullscreen)
  (blink-cursor-mode -1))

(use-package hippie-exp
  :ensure nil
  :bind
  ("M-/" . hippie-expand))

(use-package idris-mode
  :ensure t)

(use-package paren
  :ensure nil
  :config
  (add-hook 'emacs-lisp-mode-hook 'show-paren-mode))

(use-package avoid
  :ensure nil
  :config
  (mouse-avoidance-mode 'cat-and-mouse))

(use-package browse-url
  :ensure nil
  :custom
  (browse-url-browser-function 'browse-url-chromium))

(use-package helm-flyspell
  :ensure t)

(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate)
  :custom
  (dumb-jump-prefer-searcher 'rg)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package xref
  :ensure nil
  :custom
  (xref-search-program 'ripgrep))

(use-package nix-mode
  :hook (nix-mode . lsp-deferred)
  :ensure t)

(use-package lsp-nix
  :ensure lsp-mode
  :after (lsp-mode)
  :demand t
  :custom
  (lsp-nix-nil-formatter ["nixpkgs-fmt"]))

(use-package git-link
  :ensure t
  :custom
  (git-link-use-commit t)
  (git-link-consider-ssh-config t))

(use-package copy-as-format
  :ensure t
  :custom
  (copy-as-format-default "slack"))

(use-package deadgrep
  :bind (("C-c C-d" . deadgrep)
         :map deadgrep-mode-map
         ("C-c" . deadgrep-visit-result-other-window))
  :ensure t)

(use-package cus-edit
  :ensure nil
  :custom
  (custom-file (concat user-emacs-directory "custom.el"))
  :config
  (load custom-file 'noerror))

(use-package rego-mode
  :ensure t)

(use-package faces
  :ensure nil
  :config
  (set-face-attribute 'default nil :height 120))

(use-package toc-org
  :ensure t)

(use-package hcl-mode
  :ensure t)

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :custom
  (company-minimum-prefix-length 0)
  (company-idle-delay 0.2))

(use-package company-box
  :after company
  :ensure t
  :hook (company-mode . company-box-mode))

;; Use pandoc-main-hydra/body
(use-package pandoc-mode
  :ensure t)

(use-package gnuplot
  :ensure t)

(use-package fzf
  :ensure t)

(use-package dockerfile-mode
  :ensure t
  :after (lsp-dockerfile)
  :hook (dockerfile-mode . lsp-deferred))

(use-package lsp-dockerfile
  :ensure lsp-mode
  :after (lsp-mode)
  :demand t)

(use-package org-make-toc
  :ensure t)

(use-package go-mode
  :ensure t
  :hook (go-mode . lsp-deferred))

(use-package lsp-go
  :ensure lsp-mode
  :after lsp-mode
  :demand t)

(use-package auth-source
  :ensure nil
  :custom
  (auth-sources '((:source "~/.authinfo.gpg"))))

(use-package forge
  :after magit
  :ensure t)

(use-package protobuf-mode
  :ensure t)

(use-package langtool
  :ensure t
  :config
  (customize-set-variable 'langtool-bin "languagetool-commandline")
  (customize-set-variable 'langtool-default-language "en-US"))

(use-package package-lint
  :ensure t)

(use-package helpful
  :ensure t
  :bind
  (("<f1>" . help-command)
   ("C-h F" . helpful-function)
   ("C-h k" . helpful-key)
   ("C-h v" . helpful-variable))
  :init
  ;; Use shell-like backspace C-h, rebind help to F1
  (define-key key-translation-map [?\C-h] [?\C-?]))

(use-package typescript-mode
  :ensure t
  :hook ((typescript-mode . lsp-deferred))
  :custom
  (typescript-indent-level 2))

;;; Required for typescript
(use-package lsp-javascript
  :ensure lsp-mode
  :after lsp-mode
  :demand t)

(use-package vterm
  :custom
  (vterm-shell "fish"))

(use-package bison-mode
  :ensure t)

(use-package pdf-tools
  :init (pdf-tools-install))

(use-package php-mode
  :ensure t)

(use-package conf-mode
  :ensure nil
  :after (lsp-mode)
  :hook (conf-toml-mode . lsp-deferred))

(defun sibi-custom-json-hook ()
  "Custom hook to save spaces in json file."
  (make-local-variable 'tab-width)
  (make-local-variable 'js-indent-level)
  (setq tab-width 2)
  (setq js-indent-level 2))

(use-package json-mode
  :ensure t
  :after lsp-mode
  :hook ((json-mode . lsp-deferred)
	 (json-mode . sibi-custom-json-hook)))

(use-package lsp-json
  :ensure lsp-mode
  :after json-mode
  :demand t)

(use-package jinx
  :ensure nil
  :bind (:map jinx-mode-map
	      ("C-c f g" . jinx-correct)
              ("C-c f n" . jinx-next)
              ("C-c f p" . jinx-previous)))

;; Best to have it at bottom
;; https://github.com/Kungsgeten/selected.el#installation-and-setup
(use-package selected
  :ensure t
  :commands selected-minor-mode
  :init (selected-global-mode)
  :bind (:map selected-keymap
              ("w" . er/expand-region)
              ("q" . selected-off)
              ("u" . upcase-region)
              ("d" . downcase-region)
              ("g" . google-this-noconfirm)
              ("G" . google-this)
              ("m" . apply-macro-to-region-lines)))

;; (use-package lsp-java
;;   :ensure t
;;   :after lsp-mode
;;   :custom
;;   (lsp-java-server-config-dir "/home/sibi/config_linux")
;;   (lsp-java-server-install-dir (expand-file-name "share/java" (file-name-directory (directory-file-name (file-name-directory (file-name-directory (file-truename (executable-find "jdt-language-server"))))))))
;;   :hook (java-mode . lsp-deferred))

(use-package dart-mode
  :ensure t)

(use-package flycheck-vale
  :ensure t)

(use-package solidity-mode
  :ensure t)

(use-package combobulate
  :quelpa (combobulate :fetcher github
                       :repo "mickeynp/combobulate"))

(use-package ellama
  :ensure t)

(load-file "~/.emacs.d/haskell.el")
(load-file "~/.emacs.d/sibi-utils.el")
(load-file "~/.emacs.d/org.el")
(load-file "~/github/dotfiles/.emacs.d/devops.el")  ; Have this at the end because of envrc
