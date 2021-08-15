(use-package just-mode
  :ensure t)

(use-package kubel
  :ensure t
  :custom
  (kubel-use-namespace-list 'on))

(use-package docker
  :ensure t
  :bind ("C-c d" . docker))
