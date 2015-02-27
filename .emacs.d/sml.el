(use-package sml-mode
  :config
  (progn
    (defun sibi-sml-customization ()
      (define-key sml-mode-map (kbd "C-'") 'sml-prog-proc-switch-to))

    (add-hook 'sml-mode-hook 'sibi-sml-customization)))
