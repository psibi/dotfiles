(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node"
  :config
  (progn
    (use-package skewer-mode
      :ensure t
      :diminish skewer-mode
      :init
      (progn
        (add-hook 'js2-mode-hook 'skewer-mode)
        (add-hook 'js2-mode-hook '(auto-fill-mode -1))))))

(use-package web-mode
  :ensure t
  :mode "\\.jsx$"
  :config
  (progn
    (flycheck-mode 1)
    (defun sibi-web-hook ()
      (setq web-mode-code-indent-offset 4)
      (setq web-mode-markup-indent-offset 4)
      (flycheck-mode 1))
    (add-hook 'web-mode-hook 'sibi-web-hook)
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(javascript-jshint)))
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (setq-default flycheck-disabled-checkers
                  (append flycheck-disabled-checkers
                          '(json-jsonlist)))))

(use-package html-mode
  :mode "\\.html\\'"
  :bind ("C-'" . skewer-repl)
  :init
  (progn
    (add-hook 'html-mode-hook 'turn-off-auto-fill)
    (add-hook 'html-mode-hook 'skewer-mode)
    (add-hook 'html-mode-hook (lambda ()
                                (setq sgml-basic-offset 4)
                                (setq indent-tabs-mode t)))))

(use-package json-mode
  :ensure t
  :mode "\\.json\\'")
