(use-package just-mode
  :ensure t
  :bind
  (:map just-mode-map
	("C-c C-j" . justl)
	("C-c C-r" . justl-exec-default-recipe)
	:map justl-compile-mode-map
	("C-c C-j" . justl)))

(use-package kubel
  ;; :ensure t
    :quelpa (kubel :fetcher file
                   :path "~/github/kubel/kubel.el"
                    )
  :custom
  (kubel-use-namespace-list 'on)
  (kubel-env-variables (list "KUBECONFIG" "AWS_ACCESS_KEY_ID" "AWS_SECRET_ACCESS_KEY")))

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
  ;; :ensure t
  :quelpa (justl :fetcher file
                 :path "~/github/justl.el/justl.el")
  :init
  :custom
  (justl-recipe-width 40))
