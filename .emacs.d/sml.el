(require 'sml-mode)

(add-hook 'sml-mode-hook 'sibi-sml-customization)

(defun sibi-sml-customization ()
  (define-key sml-mode-map (kbd "C-'") 'sml-prog-proc-switch-to))
