(add-hook 'python-mode-hook 'sibi-customization)

(defun sibi-customization ()
  (define-key python-mode-map (kbd "C-'") 'python-shell-switch-to-shell)
  (define-key python-mode-map (kbd "C-c C-l") 'python-shell-send-buffer)
  )
