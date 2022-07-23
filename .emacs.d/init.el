;; My Emacs configuration
;; Author: Sibi <sibi@psibi.in>
;; File path: ~/.emacs.d/init.el

;; Remove menu, tool and scroll bar.
(menu-bar-mode -1)
(tool-bar-mode -1)

(setq package-native-compile t)

(setq package-archives
      '(("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

;; Bootstrap `use-package`
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; Install and load `quelpa-use-package'.
(use-package quelpa-use-package
  :ensure t)
(require 'quelpa-use-package)

(use-package cl)
(use-package saveplace)
(use-package ffap)
(use-package uniquify)
(use-package ansi-color)
(use-package recentf)
(use-package tramp)

(use-package yaml-mode
  :mode (
         ("\\.yml\\'" . yaml-mode))
  :ensure t)

(use-package dash
  :ensure t)

;; (use-package spacemacs-theme
;;   :ensure t
;;   :defer t
;;   :init
;;   (progn
;;     (load-theme 'spacemacs-dark t)
;;     ;; (load-theme 'wheatgrass t)
;;     ;; Exploits a bug to get a better modeline
;;     ))

(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-bold t)
  (doom-themes-enable-italic t)
  :config (progn
            (load-theme 'doom-one t)
            (doom-themes-org-config)))

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
  :hook
  (prog-mode . flycheck-mode)
  :custom
  (flycheck-sh-shellcheck-executable "shellcheck")
  :config
  (progn
    (setq flycheck-check-syntax-automatically '(mode-enabled save))
    (define-key flycheck-mode-map (kbd "C-c f l") #'flycheck-list-errors)
    (define-key flycheck-mode-map (kbd "C-c f n") #'flycheck-next-error)
    (define-key flycheck-mode-map (kbd "C-c f p") #'flycheck-previous-error)
    (add-hook 'sh-mode-hook (lambda () (progn
                                         (sh-set-shell "bash")
                                         (flycheck-mode))))))

(use-package markdown-mode
  ;; :ensure t
    :quelpa (markdown-mode :fetcher file
                           :path "~/github/markdown-mode"
                           :files ("*.el"))

  :mode "\\.md\\'"
  :custom
  (markdown-hide-urls t))

(use-package markdown-toc
  :ensure t
  :custom
  (markdown-toc-indentation-space 2))

(use-package auto-complete
  :ensure t
  :diminish auto-complete-mode
  :init
  (progn
    (setq ac-ignore-case nil)
    (ac-config-default)))

(use-package tree-sitter
  :ensure t
  :config
  (progn
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)))

(use-package tree-sitter-langs
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))

  ;; :quelpa (lsp-mode :fetcher file
  ;;                   :path "~/github/lsp-mode"
  ;;                   :files ("*.el" "clients/*.el")
  ;;                   )

(use-package lsp-mode
  ;; :quelpa (lsp-mode :fetcher file
  ;;                   :path "~/github/lsp-mode"
  ;;                   :files ("*.el" "clients/*.el"))
  :ensure t
  :init (setq lsp-keymap-prefix "C-l")
  :after (dash)
  :commands lsp

  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (lsp-configure . lsp-lens-mode)
         (terraform-mode . lsp-deferred))
  :custom
  (lsp-disabled-clients '(tfls))
  (lsp-semantic-tokens-enable t)
  (lsp-semantic-tokens-honor-refresh-requests t)
  (lsp-terraform-ls-enable-show-reference t)
  (lsp-semantic-tokens-warn-on-missing-face nil)
  (lsp-terraform-ls-prefer-treemacs-all-the-icons-theme t))

(use-package lsp-ui
  :ensure t
  :after (lsp-mode)
  :commands lsp-ui-mode
  :custom
  lsp-ui-doc-enable t
  lsp-ui-doc-header t
  lsp-ui-doc-show-with-cursor t
  lsp-ui-doc-include-signature t)

(use-package treemacs-all-the-icons
  :ensure t)

(use-package lsp-treemacs
  :ensure t
  :after (treemacs lsp-mode treemacs-all-the-icons))

(use-package helm-lsp
  :ensure t
  :bind (([remap xref-find-apropos] . helm-lsp-workspace-symbol)
         ("C-c C-e g" . helm-lsp-global-workspace-symbol)
         ("C-c C-e d" . helm-lsp-diagnostics))
  :commands helm-lsp-workspace-symbol)

(use-package terraform-mode
  :ensure t
  :after (lsp-mode)
  :bind (:map terraform-mode-map
              ("C-c C-c C-f" . terraform-format-buffer)))

(use-package terraform-doc
  :ensure t)

(use-package rustic
  :quelpa (rustic :fetcher file
                  :path "~/github/rustic")
  ;; :ensure t
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-d" . deadgrep)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c o" . lsp-rust-analyzer-open-external-docs)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :hook ((rustic-mode . lsp-deferred))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (progn
    (setq rustic-analyzer-command '("~/.nix-profile/bin/rust-analyzer"))
    (setq rustic-format-on-save nil)
    (tree-sitter-require 'rust)
    (add-hook 'rustic-mode-hook #'tree-sitter-mode)
    (customize-set-variable 'lsp-rust-analyzer-proc-macro-enable t)
    (customize-set-variable 'lsp-rust-analyzer-experimental-proc-attr-macros t)
    (customize-set-variable 'lsp-rust-analyzer-server-display-inlay-hints t)))

(use-package yasnippet
  :ensure t
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

(set-scroll-bar-mode 'nil)
(size-indication-mode 1)

;; My Details
(setq user-full-name "Sibi Prabakaran")
(setq user-mail-address "sibi@psibi.in")

;; Unbind C-z
(when window-system
  (global-unset-key [(control z)]))
(setq vc-follow-symlinks t)

;; ----------------------
;; Final newline handling
;; ----------------------
(setq require-final-newline t)
(setq next-line-extends-end-of-buffer nil)
(setq next-line-add-newlines nil)
(setq use-default-font-for-symbols nil)
;; https://emacs.stackexchange.com/questions/62049/override-the-default-font-for-emoji-characters
(set-fontset-font t 'symbol "Twitter Color Emoji")

;; -------------------
;; Everything in UTF-8
;; -------------------
;; Not everything - as this stops the installation of auctex
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;;Tramp for editing protected files in existing Emacs session.(C-x C-f /sudo)
(setq tramp-default-method "ssh")

;; Custom Shortcuts
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Rebind Enter
;; (define-key global-map (kbd "C-c j") 'newline-and-indent)

(global-set-key (kbd "C-x m") 'shell)

;; Just in case you are behind a proxy
;; (setq url-proxy-services '(("https" . "127.0.0.1:3129")
;;                            ("http" . "127.0.0.1:3129")))

;; Hooks before saving file
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; (add-hook 'before-save-hook 'untabify)

;; No tabs for indendation
(setq-default indent-tabs-mode nil)
(setq tab-width 4)

;; -------------
;; flyspell-mode
;; -------------

(use-package flyspell
  :ensure t
  :init
  (progn
    (flyspell-mode 1))
  :config
  (progn
    (setq ispell-program-name "aspell")
    (setq ispell-list-command "--list") ;; run flyspell with aspell, not ispell
    ))

;; Octave-mode
(use-package octave
  :ensure t
  :mode "\\.m\\'")

(use-package magit
  :ensure t
  :init
  :bind (("C-c g" . 'magit-status))
  :custom
  (magit-auto-revert-mode 1)
  (magit-commit-arguments (quote ("--gpg-sign=BB557613"))))

;; (use-package magithub
;;   :ensure t
;;   :after magit
;;   :config
;;   (progn
;;     (magithub-feature-autoinject t)
;;     (magithub-ci-disable)
;;     ;; https://github.com/vermiculus/magithub/issues/75#issuecomment-284256987
;;     (defun magithub--api-available-p ()
;;       't
;;       )
;;     ;; (magithub-toggle-issues)
;;     ;; (magithub-toggle-pull-requests)
;;     ))


;; https://www.reddit.com/r/emacs/comments/sunkzx/is_it_only_me_or_did_emacs_without_native_comp/hxbthxv/?utm_source=reddit&utm_medium=web2x&context=3
;; (setq gc-cons-threshold 20000000)

;;Projectile related config
(use-package projectile
  :ensure t
  :diminish projectile-mode "p"
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (progn
    (projectile-global-mode))
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
  :config
  (progn
    (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (global-unset-key (kbd "C-x c"))
    (global-set-key (kbd "C-x p") 'helm-list-elisp-packages-no-fetch)
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z") 'helm-select-action) ; list actions using C-z
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))
    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
          helm-buffers-fuzzy-matching           t ; fuzzy matching buffer names when non--nil
          helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
          helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
          helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
          helm-ff-file-name-history-use-recentf t
          helm-ff-newfile-prompt-p nil)
    (global-set-key (kbd "M-x") 'helm-M-x)
    (global-set-key "\C-x\C-m" 'helm-M-x)
    (global-set-key (kbd "M-y") 'helm-show-kill-ring)
    (global-set-key (kbd "C-x b") 'helm-mini)
    (global-set-key (kbd "C-x C-f") 'helm-find-files)
    (global-set-key (kbd "C-c h o") 'helm-occur)))

;; (use-package imenu-anywhere
;;   :ensure t)

(use-package helm-projectile
  :ensure t
  :init
  (progn
    (helm-projectile-on))
  :custom
  (projectile-completion-system 'helm))

(use-package helm-swoop
  :ensure t
  :config
  (progn
    (global-set-key (kbd "M-i") 'helm-swoop)
    (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
    (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
    (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)

    ;; When doing isearch, hand the word over to helm-swoop
    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
    ;; From helm-swoop to helm-multi-swoop-all
    (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
    ;; When doing evil-search, hand the word over to helm-swoop
    ;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

    ;; Move up and down like isearch
    (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
    (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
    (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)

    ;; Save buffer when helm-multi-swoop-edit complete
    (setq helm-multi-swoop-edit-save t)

    ;; If this value is t, split window inside the current window
    (setq helm-swoop-split-with-multiple-windows nil)

    ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
    (setq helm-swoop-split-direction 'split-window-vertically)

    ;; If nil, you can slightly boost invoke speed in exchange for text color
    (setq helm-swoop-speed-or-color nil)

    ;; ;; Go to the opposite side of line from the end or beginning of line
    (setq helm-swoop-move-to-line-cycle t)

    ;; Optional face for line numbers
    ;; Face name is `helm-swoop-line-number-face`
    (setq helm-swoop-use-line-number-face t)))

(use-package doc-view
  :ensure t
  :config
  (progn
    (add-hook 'doc-view-mode-hook 'auto-revert-mode)))

(use-package ace-window
  :ensure t
  :init
  (progn
    (global-set-key (kbd "C-x o") 'ace-window)))

;; (use-package smart-mode-line
;;   :ensure t
;;   :init
;;   (progn
;;     (setq sml/no-confirm-load-theme t)
;;     (sml/setup)))

;; (use-package smart-mode-line-powerline-theme
;;   :ensure t
;;   :init
;;   (progn
;;     (setq sml/theme 'powerline)
;;     (setq powerline-arrow-shape 'curve)
;;     (setq powerline-default-separator-dir '(right . left))))

;; Enable clipboard
(setq x-select-enable-clipboard t)

;; Dired is better with human readable format
(setq dired-listing-switches "-alh")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom splitting functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(global-set-key (kbd "C-x 2") 'vsplit-last-buffer)
(global-set-key (kbd "C-x 3") 'hsplit-last-buffer)

;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)

;; (add-to-list 'load-path "~/.emacs.d/site-lisp/auctex")
;; (load "auctex.el" nil t t)
;; (load "preview-latex.el" nil t t)

(use-package tex-mode
  :ensure t
  :defer t
  :config
  (progn
    (defun complete-dollar ()
      "Auto completes dollar symbol"
      (interactive)
      (insert "$$")
      (backward-char 1))
    (defun sibi-latex-keys ()
      "Modify keymaps by latex-mode"
      (local-set-key (kbd "$") 'complete-dollar))
    (add-hook 'LaTeX-mode-hook 'sibi-latex-keys)))

(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :init
  (progn
    (setq guide-key/guide-key-sequence '("C-x 4" "C-c p"))
    (guide-key-mode 1)))

(use-package window-numbering
  :ensure t
  :init
  (progn
    (window-numbering-mode t)))

;; (use-package spaceline
;;   :ensure t
;;   :init
;;   (progn
;;     (require 'spaceline-config)
;;     (spaceline-emacs-theme)))

(use-package ace-jump-mode
  :ensure t
  :init
  (progn
    (define-key global-map (kbd "C-c SPC") 'ace-jump-mode)
    ;; Also with universal argument: C-u C-c SPC
    ;; For line jumping: C-u C-u C-c SPC
    ))

(use-package nyan-mode
  :ensure t
  :init
  (progn
    (nyan-mode 1)))

(use-package flycheck-pos-tip
  :ensure t
  :init
  (progn
    (with-eval-after-load 'flycheck
      (flycheck-pos-tip-mode))))

(use-package electric
  :ensure t
  :init
  (progn
    (electric-pair-mode 1)))

;; Make sure to call (all-the-icons-install-fonts) once
;; (all-the-icons-install-fonts)
(use-package all-the-icons
  :ensure t)


;; (use-package mode-icons
;;   :ensure t
;;   :init
;;   (when (display-graphic-p) (mode-icons-mode)))

(use-package expand-region
  :ensure t)

(use-package centered-cursor-mode
  :ensure t
  :init
  (progn (require 'centered-cursor-mode)
         (global-centered-cursor-mode)))

(use-package tldr
  :ensure t)

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;; Shift the selected region right if distance is postive, left if
;; negative

(defun shift-region (distance)
  (let ((mark (mark)))
    (save-excursion
      (indent-rigidly (region-beginning) (region-end) distance)
      (push-mark mark t t)
      ;; Tell the command loop not to deactivate the mark
      ;; for transient mark mode
      (setq deactivate-mark nil))))

(defun shift-right ()
  (interactive)
  (shift-region 1))

(defun shift-left ()
  (interactive)
  (shift-region -1))

;; Bind (shift-right) and (shift-left) function to your favorite keys. I use
;; the following so that Ctrl-Shift-Right Arrow moves selected text one
;; column to the right, Ctrl-Shift-Left Arrow moves selected text one
;; column to the left:

(global-set-key (kbd "C-c >") 'shift-right)
(global-set-key (kbd "C-c <") 'shift-left)

;; Isearch convenience, space matches anything (non-greedy)
;; Note that you can use universal argument
(setq search-whitespace-regexp ".*?")
(toggle-frame-fullscreen)
(global-set-key (kbd "M-/") 'hippie-expand)

(fset 'yes-or-no-p 'y-or-n-p)
(setq confirm-nonexistent-file-or-buffer nil)

(use-package rg
  :ensure t)

(use-package idris-mode
  :ensure t
  :mode "\\.idr\\'")

(if (eq system-type 'darwin)
    (progn
      (set-face-attribute 'default nil :height 150)))

(show-paren-mode 1)
;; For proof related
;; (load "/Users/sibi/github/PG/generic/proof-site.el")
;; (load "/home/sibi/Downloads/ProofGeneral-4.2/generic/proof-site.el")

;; Backup related
(setq backup-directory-alist '(("" . "~/.emacs.d/emacs_backup"))
      backup-by-copying t
      version-control t
      kept-old-versions 2
      kept-new-versions 20
      delete-old-versions t)
(setq tramp-backup-directory-alist backup-directory-alist)
(setq auto-save-file-name-transforms nil)

(global-auto-revert-mode 1)
(blink-cursor-mode -1)

(mouse-avoidance-mode 'cat-and-mouse)

(setq inhibit-startup-message t)

(setq browse-url-browser-function 'browse-url-chrome)

(use-package helm-flyspell
  :ensure t
  :config ())

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
  :ensure t)

(use-package git-gutter
  :ensure t)
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


(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (all-the-icons dockerfile-mode org-mode yaml-mode window-numbering which-key web-mode use-package tldr terraform-mode spacemacs-theme spaceline selected rg rego-mode prettier-js paredit package-lint ox-twbs org-journal nyan-mode nix-mode magit lsp-ui lsp-haskell keychain-environment json-mode jedi idris-mode hlint-refactor hindent helm-swoop helm-projectile helm-lsp helm-flyspell guide-key google-this git-link git-gutter flycheck-rust flycheck-pos-tip expand-region emms editorconfig dumb-jump dhall-mode deadgrep copy-as-format centered-cursor-mode cargo aggressive-indent ace-window ace-jump-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; (face-attribute 'default :font)



(use-package rego-mode
  :ensure t
  :custom
  (rego-repl-executable "/home/sibi/bin/opa")
  (regg-opa-command "/home/sibi/bin/opa"))

(set-face-attribute 'default nil :height 120)

(use-package toc-org
  :ensure t)

(use-package hcl-mode
  :ensure t)


(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-box
  :after company
  :ensure t
  :hook (company-mode . company-box-mode))


;; (use-package company-suggest
;;   :ensure t
;;   :init
;;   (add-to-list 'company-backends 'company-suggest-google
;;   (add-to-list 'company-backends 'company-suggest-wiktionary)))

;; Use pandoc-main-hydra/body
(use-package pandoc-mode
  :ensure t)

(use-package find-file-in-project
  :ensure t
  :custom
  (ffip-use-rust-fd t))

(use-package xwwp
  :ensure t)

(use-package xwwp-follow-link-helm
  :ensure t
  :custom
  (xwwp-follow-link-completion-system 'helm)
  :bind (:map xwidget-webkit-mode-map ("v" . xwwp-follow-link)))

(use-package xwidgete
  :ensure t)

;; (use-package java-mode
;;   :custom
;;   )

;; (add-to-list 'load-path (concat (getenv "JAVA_HOME") "/bin"))

(add-to-list 'auto-mode-alist '("\\.shell-session\\'" . sh-mode))

(use-package gnuplot
  :ensure t)

(use-package fzf
  :ensure t)

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(use-package dockerfile-mode
  :ensure t)

(use-package org-make-toc
  :ensure t)

(use-package dogears
  :quelpa (dogears :fetcher github :repo "alphapapa/dogears.el")

  :init (dogears-mode)
  ;; These bindings are optional, of course:
  :bind (:map global-map
              ("M-g d" . dogears-go)
              ("M-g M-b" . dogears-back)
              ("M-g M-f" . dogears-forward)
              ("M-g M-d" . dogears-list)
              ("M-g M-D" . dogears-sidebar)))

(use-package go-mode
  :ensure t)

(use-package forge
  :after magit
  :ensure t
  :config
  (setq auth-sources '((:source "~/.authinfo.gpg")))
  (setq gitlab.user "sibi"))

(use-package prodigy
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
  :ensure t)

(use-package doom-modeline
  :ensure t
  :config (doom-modeline-mode 1))

(use-package typescript-mode
  :ensure t
  :custom
  (typescript-indent-level 2))
;;; Workaround to make GPG agent work well with ssh keys till I move
;;; managing my xmonad configuration with home manager itself

;; https://github.com/nix-community/home-manager/issues/307
;; https://nix-community.github.io/home-manager/index.html#sec-install-standalone
;; https://github.com/nix-community/home-manager/issues/292#issuecomment-403104476
;; We do this to setup GPG integration properly

(setenv "SSH_AUTH_SOCK"
        (string-trim (shell-command-to-string "gpgconf --list-dirs agent-ssh-socket")))


(load-file "~/.emacs.d/haskell.el")
(load-file "~/.emacs.d/python.el")
(load-file "~/.emacs.d/web.el")
(load-file "~/.emacs.d/sibi-utils.el")
(load-file "~/.emacs.d/org.el")
(load-file "~/github/dotfiles/.emacs.d/devops.el")
;; (load-file "~/github/dotfiles/.emacs.d/private.el")
;; (load-file "~/.emacs.d/sml.el")
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; https://emacs.stackexchange.com/a/13096
(defun sibi-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

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

(use-package bison-mode
  :ensure t)

(use-package pdf-tools
  :init (pdf-tools-install))
