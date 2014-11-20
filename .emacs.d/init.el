;; My Emacs configuration
;; Author: Sibi <sibi@psibi.in>
;; File path: ~/.emacs.d/init.el
(server-start)

(package-initialize)

(load-theme 'wheatgrass t)
(require 'cl)
(require 'saveplace)
(require 'ffap)
(require 'uniquify)
(require 'ansi-color)
(require 'recentf)
(require 'package)
(require 'tramp)
(require 'auto-complete)
(require 'autopair)
(require 'auto-complete-config)
(require 'fullscreen-mode)
(require 'google-this)
(require 'imenu-anywhere)
(require 'haskell-mode)
(require 'magit)
(require 'flx-ido)
(require 'projectile)
(require 'helm-config)
(require 'ace-window)
(require 'helm-projectile)

(load-file "~/.emacs.d/haskell.el")
(load-file "~/.emacs.d/python.el")
(load-file "~/.emacs.d/web.el")
(load-file "~/.emacs.d/sibi-utils.el")
;(load-file "~/.emacs.d/sml.el")

(setq package-archives
      '(("gnu"         . "http://elpa.gnu.org/packages/")
        ("original"    . "http://tromey.com/elpa/")
        ("org"         . "http://orgmode.org/elpa/")
        ("marmalade"   . "http://marmalade-repo.org/packages/")
        ("melpa" . "http://melpa.milkbox.net/packages/")))

;; Remove menu, tool and scroll bar.
(menu-bar-mode -1)
(tool-bar-mode -1)
(set-scroll-bar-mode 'nil)
(size-indication-mode 1)

;; Unbind C-z
(when window-system
  (global-unset-key [(control z)]))

;; Make fullscreen
(fullscreen-mode-fullscreen)

;; ----------------------
;; Final newline handling
;; ----------------------
(setq require-final-newline t)
(setq next-line-extends-end-of-buffer nil)
(setq next-line-add-newlines nil)

;; -------------------
;; Everything in UTF-8
;; -------------------
(prefer-coding-system                   'utf-8)
(set-language-environment               'utf-8)
(set-default-coding-systems             'utf-8)
(setq file-name-coding-system           'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq coding-system-for-write           'utf-8)
(set-keyboard-coding-system             'utf-8)
(set-terminal-coding-system             'utf-8)
(set-clipboard-coding-system            'utf-8)
(set-selection-coding-system            'utf-8)
(setq default-process-coding-system     '(utf-8 . utf-8))
(add-to-list 'auto-coding-alist         '("." . utf-8))

(ac-config-default)
(autopair-global-mode) ;; enable autopair in all buffers

;;Tramp for editing protected files in existing Emacs session.(C-x C-f /sudo)
(setq tramp-default-method "ssh")

;;Python Development Environment
;;Install jedi for Auto-completion in Python mode. For key bindings see: C-h v jedi:setup-keys
;; Install 3 Python dependencies: sudo pip install jedi epc argparse
;; Install 3 Emacs ELPA packages: epc deferred auto-complete
;; Clone a directory named jedi into elpa from: git://github.com/tkf/emacs-jedi.git
(add-to-list 'load-path "~/.emacs.d/elpa/jedi")
(autoload 'jedi:setup "jedi" nil t)
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:setup-keys t)

;; Custom Shortcuts
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

;; Package List key binding
(global-set-key (kbd "C-x p") 'package-list-packages-no-fetch)
;; Rebind Enter
(define-key global-map (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "C-x m") 'shell)

;; Emacs doesn't seem to have `copy-rectangle-as-kill`
;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Rectangles.html
(defun my-copy-rectangle (start end)
   "Copy the region-rectangle instead of `kill-rectangle'."
   (interactive "r")
   (setq killed-rectangle (extract-rectangle start end)))
 
(global-set-key (kbd "C-x r M-w") 'my-copy-rectangle)

;; Just in case you are behind a proxy
;; (setq url-proxy-services '(("https" . "127.0.0.1:3129")
;;                            ("http" . "127.0.0.1:3129")))

;; -------------
;; flyspell-mode
;; -------------
(setq ispell-program-name "aspell")
(setq ispell-list-command "--list") ;; run flyspell with aspell, not ispell

;; Octave-mode
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; emms
(require 'emms-setup)
(emms-standard)
(emms-default-players)

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1) 
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)

(setq gc-cons-threshold 20000000)
(magit-auto-revert-mode 1)

;;Projectile related config
(projectile-global-mode)
(setq projectile-enable-caching t)

;;Helm related config
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-unset-key (kbd "C-x c"))

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
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key "\C-x\C-m" 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-c h o") 'helm-occur)

;;Latex
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(global-set-key (kbd "C-x o") 'ace-window)
