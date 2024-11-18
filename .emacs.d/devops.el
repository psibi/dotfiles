(use-package just-mode
  :ensure t
  :bind
  (:map just-mode-map
	("C-c C-j" . justl)
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

(use-package ox-reveal
  :ensure t)

(use-package justl
  :ensure t
  :init
  :custom
  (justl-recipe-width 40)
  (justl-include-private-recipes nil))
