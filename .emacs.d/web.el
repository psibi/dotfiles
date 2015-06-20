(use-package js2-mode
  :ensure t
  :mode "\\.js\\'"
  :interpreter "node"
  :config
  (progn
    (use-package skewer-mode
      :ensure t
      :init
      (progn
        (add-hook 'js2-mode-hook 'skewer-mode)))))

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
