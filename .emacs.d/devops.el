(use-package just-mode
  :ensure t
  :bind
  (:map just-mode-map
	("C-c C-j" . justl)
	("C-l = =" . just-format-buffer)
	("C-c C-r" . justl-exec-default-recipe)
	:map justl-compile-mode-map
	("C-c C-j" . justl)))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))

(use-package envrc
  :ensure t
  :init
  (envrc-global-mode))

;; (use-package ox-reveal
;;   :ensure t)

(use-package justl
  :ensure t
  ;; :quelpa (justl :fetcher file
  ;;                :path "~/github/justl.el/"
  ;;                :files ("justl.el"))
  :custom
  (justl-recipe-width 40)
  (justl-shell 'eshell)
  (justl-pop-to-buffer-on-display t)
  (justl-include-private-recipes nil))

;; (use-package justl
;;   :ensure t
;;   :custom
;;   (justl-recipe-width 40)
;;   (justl-pop-to-buffer-on-display nil)
;;   (justl-include-private-recipes nil))
