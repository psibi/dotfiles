(require 'python-mode)
(require 'ipython)
(add-hook 'python-mode-hook 'sibi-customization)

(setenv "PYTHONPATH" ".:..")

(defun sibi-customization ()
  (define-key python-mode-map (kbd "C-'") 'sibi-python-shell)
  (define-key python-mode-map (kbd "C-c C-l") 'sibi-python-load-buffer))

(defun sibi-python-load-buffer ()
  (interactive)
  (progn
    (setq cur-buf-name (buffer-name))
    (python-shell-switch-to-shell)
    (erase-buffer)
    (switch-to-buffer-other-window cur-buf-name)
    (python-shell-send-buffer)))

(defun sibi-python-shell ()
  (interactive)
  (progn
    (setq cur-buf-name (buffer-name))
    (python-shell-switch-to-shell)
    (switch-to-buffer-other-window cur-buf-name)))
