;; My Emacs configuration
;; Author: Sibi <sibi@psibi.in>
;; File path: ~/.emacs.d/init.el


(setq package-archives '(("gnu"   . "http://elpa.gnu.org/packages/")
                        ("melpa" . "https://melpa.org/packages/")))

(require 'use-package)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(setq sibi-packages '(use-package))
(dolist (package sibi-packages)
  (unless (package-installed-p package)
   (package-install package)))

;;; https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq user-mail-address "sibi@psibi.in")
(setq user-full-name "Sibi Prabakaran")

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

(use-package comp
  :ensure nil
  :custom
  ;; https://github.com/jrblevin/markdown-mode/issues/578#issuecomment-1126380098
  (native-comp-deferred-compilation-deny-list '("markdown-mode\\.el$")))

(use-package quelpa-use-package
  :ensure t)

(use-package dash
  :ensure t)

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

(use-package treemacs
  :ensure t
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

(use-package markdown-toc
  :ensure t
  :custom
  (markdown-toc-indentation-space 2))

(use-package markdown-mode
  :ensure t
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
  :ensure t
  :init (setq lsp-keymap-prefix "C-l")
  :after (dash)
  :commands lsp
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-configure . lsp-lens-mode))
  :custom
  (lsp-disabled-clients '(tfls clangd rls))
  ;; (lsp-log-io t)
  (lsp-log-io nil)
  (lsp-semantic-tokens-enable t)
  (lsp-lens-auto-enable t)
  (lsp-semantic-tokens-honor-refresh-requests nil)
  (lsp-semantic-tokens-allow-delta-requests t)
  (lsp-semantic-tokens-allow-ranged-requests t)
  (lsp-semantic-tokens-warn-on-missing-face nil))

(use-package lsp-terraform
  :ensure lsp-mode
  :after lsp-mode
  :demand t
  :custom
  (lsp-terraform-ls-enable-show-reference t)
  (lsp-terraform-ls-prefer-treemacs-all-the-icons-theme t))

(use-package lsp-rust
  :ensure lsp-mode
  :after lsp-mode
  :demand t
  :custom
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-experimental-proc-attr-macros t)
  (lsp-rust-analyzer-proc-macro-enable t))

(use-package ccls
  :ensure t
  :after (lsp-mode)
  :hook ((c-mode c++-mode) . (lambda () (require 'ccls) (lsp))))

(use-package yaml-mode
  :ensure t
  :after (lsp-mode)
  :mode (("\\.yml\\'" . yaml-mode))
  :hook ((yaml-mode . lsp-deferred)))

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-header t)
  (lsp-ui-doc-show-with-cursor t)
  (lsp-ui-doc-include-signature t))

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

(use-package rustic
  ;; :quelpa (rustic :fetcher file
  ;;                 :path "~/github/rustic")
  :after (lsp-mode)
  :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ([remap rustic-cargo-init] . rustic-cargo-install)
              ("C-c C-c C-i" . rustic-cargo-install)
              ("C-c C-c <tab>" . rustic-cargo-build)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c o" . lsp-rust-analyzer-open-external-docs)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :hook ((rustic-mode . lsp-deferred))
  :config
  (setq rustic-format-on-save nil)
  :custom
  (rustic-analyzer-command '("rust-analyzer"))
  (rustic-default-clippy-arguments nil)
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
  :ensure t
  :hook ((rustic-mode . turn-on-smartparens-strict-mode)))

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
  (magit-commit-arguments (quote ("--gpg-sign=BB557613"))))

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
  (progn
    (require 'helm-config)
    (helm-mode 1))
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

(use-package window-numbering
  :ensure t
  :init
  (progn
    (window-numbering-mode t)))

(use-package ace-jump-mode
  :ensure t
  :bind
  ;; Also with universal argument: C-u C-c SPC
  ;; For line jumping: C-u C-u C-c SPC
  (("C-c SPC" . ace-jump-mode)))

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode 1))

(use-package flycheck-pos-tip
  :ensure t
  :after (flycheck)
  :config
  (flycheck-pos-tip-mode))

;; Make sure to call (all-the-icons-install-fonts) once
;; (all-the-icons-install-fonts)
(use-package all-the-icons
  :ensure t)

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
  (browse-url-browser-function 'browse-url-chrome))

(use-package helm-flyspell
  :ensure t)

(use-package dumb-jump
  :ensure t
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g b" . dumb-jump-back))
  :config (progn
            (setq dumb-jump-selector 'helm)
            (setq dumb-jump-force-searcher 'rg)))

(use-package nix-mode
  :ensure t)

(use-package git-link
  :quelpa (git-link :fetcher file
                    :path "~/github/git-link/git-link.el")

  ;; :ensure t
  :custom
  (git-link-use-commit t))

(use-package copy-as-format
  :ensure t
  :custom
  (copy-as-format-default "slack"))

(use-package deadgrep
  :bind (("C-c C-d" . deadgrep)
         :map deadgrep-mode-map
         ("C-c" . deadgrep-visit-result-other-window))
  :ensure t
  :config
  (add-hook 'xref-backend-functions 'dumb-jump-xref-activate))

;; Refresher:
;; C-c r - Root file in HELM
;; C-c h o - Helm occur
;; C-c h i - helm-imenu
;; C-c h b - Resume previous helm session
;; C- Spc : Marks
;; C-c C-i  : insert mark content

;; Recusively find & replace in text files
;; M-x find-name-dired
;; t, Q, query-replace-regexp

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
  (company-minimum-prefix-length 0))

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
  :ensure t)

(use-package org-make-toc
  :ensure t)

(use-package go-mode
  :ensure t)

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

(use-package nixpkgs-fmt
  :ensure t)

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

(use-package doom-modeline
  :ensure t
  :config (doom-modeline-mode 1))

(use-package typescript-mode
  :ensure t
  :custom
  (typescript-indent-level 2))

(use-package vterm
  :custom
  (vterm-shell "fish"))

(use-package bison-mode
  :ensure t)

(use-package pdf-tools
  :init (pdf-tools-install))

(use-package php-mode
  :ensure t)

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

(load-file "~/.emacs.d/haskell.el")
(load-file "~/.emacs.d/sibi-utils.el")
(load-file "~/.emacs.d/org.el")
(load-file "~/github/dotfiles/.emacs.d/devops.el")  ; Have this at the end because of envrc
