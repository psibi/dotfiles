(use-package just-mode
  :ensure t)

(use-package kubel
  :ensure t
  :custom
  (kubel-use-namespace-list 'on))

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
  :quelpa (justl :fetcher file
                 :path "~/github/justl.el/justl.el")
  :custom
  (justl-recipe-width 40))
